module Page.Solve exposing (..)

import Html exposing (Html, div, span, text, button, ul, ol, li)
import Html.Attributes exposing (id, class, title, disabled, style)
import Html.Events exposing (onClick)
import Utilities exposing (castHtml, isNothing, fromJust, remove, scrollToTopCmd, scrollToBottomCmd, unsafeGet, ariaHidden)
import Problem exposing (Problem)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline
import Graphics
import Render
import Rule
import Svg
import Svg.Attributes as SA
import Path exposing (Path, PathNode(GoLeft, GoRight))
import Term exposing (..)
import Dialog.Number
import Dialog.Scratch
import Dialog.Transformation
import Transformation exposing (Transformation)
import Transformations
import Task
import Ports
import Step exposing (Step)


type alias Model =
    { problemIndex : Int
    , direction : Direction
    , historyIndex : Int
    , futureIndex : Int
    , historyPath : Path
    , futurePath : Path
    , panel : Panel
    , contextSelection : Maybe Int
    , scratchSelection : Maybe Int
    , transformationSelection : Maybe Int

    -- These are not saved:
    , popUp : PopUp
    , dialog : Dialog
    , goalIsHidden : Bool

    -- These are calculated:
    , term : Term
    , focus : Term
    , context : List { path : Path, term : Term }
    , path : Path
    , transformations : List Transformation
    , solved : Bool
    }


type PopUp
    = NoPopUp
    | PanelSelectPopUp
    | SettingsPopUp
    | ResetPopUp
    | CopyPopUp


type Msg
    = IgnoreMsg
    | BackMsg
    | SolvedMsg
    | FocusLeftMsg
    | FocusOutMsg
    | FocusRightMsg
    | ChangeDirectionMsg
    | ShowPanelSelectMsg
    | PanelSelectBackgroundMsg
    | PanelSelectChoiceMsg Panel
    | ShowSettingsMsg
    | SettingsBackgroundMsg
    | SettingsEmailMsg
    | SettingsCopyMsg
    | SettingsShowProofMsg
    | SettingsResetMsg
    | ResetPopUpCancelMsg
    | ResetPopUpResetMsg
    | ContextItemMsg Int
    | NumberAddMsg
    | ScratchItemMsg Int
    | ScratchCopyMsg
    | ScratchAddMsg
    | ScratchRemoveMsg
    | NumberDialogMsg Dialog.Number.Msg
    | ScratchDialogMsg Dialog.Scratch.Msg
    | TransformationItemMsg Int
    | TransformationUseMsg
    | TransformationDialogMsg Dialog.Transformation.Msg
    | TransformationUndoMsg
    | TransformationRedoMsg
    | HistoryItemMsg Int


type OutMsg
    = UnchangedOutMsg
    | ModelChangedOutMsg Model
    | ProblemChangedOutMsg Problem Model
    | SolvedOutMsg (List Step) Problem
    | ModelCmdChangedOutMsg Model (Cmd Msg)
    | ExitOutMsg
    | ShowProofOutMsg



-- This is used to store the active panel in the model.


type Panel
    = NotesPanel
    | ContextPanel
    | ScratchPanel
    | TransformationsPanel
    | HistoryPanel
    | RulesPanel


type Dialog
    = NoDialog
    | TransformationDialog Dialog.Transformation.Model
    | NumberDialog Dialog.Number.Model
    | ScratchDialog Dialog.Scratch.Model


type Direction
    = History
    | Future


modelAriaHidden : Model -> Html.Attribute msg
modelAriaHidden model =
    case model.popUp of
        NoPopUp ->
            ariaHidden False

        _ ->
            ariaHidden True


futureStep : a -> List { d | isReversed : b, rule : c, term : a } -> List { isReversed : b, path : List e, rule : c, term : a }
futureStep t xs =
    case xs of
        [] ->
            []

        [ x1 ] ->
            [ { rule = x1.rule, isReversed = x1.isReversed, term = t, path = [] } ]

        x3 :: (x2 :: rest) ->
            { rule = x3.rule, isReversed = x3.isReversed, term = x2.term, path = [] } :: futureStep t (x2 :: rest)


historyStep : Step -> List Step -> List Step
historyStep x r =
    x :: r


proofSteps : Problem -> Model -> List Step
proofSteps problem model =
    List.foldl historyStep [] (List.drop model.historyIndex problem.history)
        ++ futureStep problem.finish (List.drop model.futureIndex problem.future)


termReplace : List PathNode -> Term -> Term -> Term
termReplace path new term =
    case path of
        [] ->
            new

        GoLeft :: rest ->
            case term of
                Binary op t1 t2 ->
                    Binary op (termReplace rest new t1) t2

                Unary op1 t3 ->
                    Unary op1 (termReplace rest new t3)

                Atom _ ->
                    Debug.crash "impossible"

        GoRight :: rest1 ->
            case term of
                Binary op2 t4 t5 ->
                    Binary op2 t4 (termReplace rest1 new t5)

                Unary op3 t6 ->
                    Unary op3 (termReplace rest1 new t6)

                Atom _ ->
                    Debug.crash "impossible"


currentTransformation : Model -> Transformation
currentTransformation model =
    case model.transformationSelection of
        Nothing ->
            Debug.crash "impossible"

        Just idx ->
            Utilities.unsafeGet idx model.transformations


transformationIsMultiple : Model -> Bool
transformationIsMultiple model =
    case model.transformationSelection of
        Nothing ->
            False

        Just idx ->
            (Utilities.unsafeGet idx model.transformations).isMultiple


termFocus : List PathNode -> Term -> Term
termFocus path term =
    case path of
        [] ->
            term

        GoLeft :: rest ->
            case term of
                Binary _ t1 _ ->
                    termFocus rest t1

                Unary _ t2 ->
                    termFocus rest t2

                Atom _ ->
                    Debug.crash "impossible"

        GoRight :: rest1 ->
            case term of
                Binary _ _ t3 ->
                    termFocus rest1 t3

                Unary _ t4 ->
                    termFocus rest1 t4

                Atom _ ->
                    Debug.crash "impossible"


pushContextRight : Path -> Term -> List { path : Path, term : Term } -> List { path : Path, term : Term }
pushContextRight path term ctx =
    case term of
        -- Atom Top ->
        --     ctx
        Binary Conjunction t1 t2 ->
            pushContextRight (GoRight :: path) t2 (pushContextRight (GoLeft :: path) t1 ctx)

        _ ->
            { path = List.reverse path, term = term } :: (List.filter (\x -> x.term /= term) ctx)


pushContextLeft : Path -> Term -> List { path : Path, term : Term } -> List { path : Path, term : Term }
pushContextLeft path term ctx =
    case term of
        -- Atom Top ->
        --     ctx
        Binary Conjunction t1 t2 ->
            pushContextLeft (GoLeft :: path) t1 (pushContextLeft (GoRight :: path) t2 ctx)

        _ ->
            { path = List.reverse path, term = term } :: (List.filter (\x -> x.term /= term) ctx)


pushNestedContext : List PathNode -> Term -> List PathNode -> List { path : Path, term : Term } -> List { path : Path, term : Term }
pushNestedContext path term pathR ctx =
    case path of
        [] ->
            ctx

        GoLeft :: rest ->
            case term of
                Binary Conjunction t1 t2 ->
                    pushNestedContext rest t1 (GoLeft :: pathR) (pushContextLeft (GoRight :: pathR) t2 ctx)

                Binary Disjunction t3 _ ->
                    pushNestedContext rest t3 (GoLeft :: pathR) ctx

                Binary Equivalence t4 _ ->
                    pushNestedContext rest t4 (GoLeft :: pathR) ctx

                Binary Implication t5 _ ->
                    pushNestedContext rest t5 (GoLeft :: pathR) ctx

                Binary EqualTo t6 _ ->
                    pushNestedContext rest t6 (GoLeft :: pathR) ctx

                Binary NotEqualTo t7 _ ->
                    pushNestedContext rest t7 (GoLeft :: pathR) ctx

                Binary Addition t8 _ ->
                    pushNestedContext rest t8 (GoLeft :: pathR) ctx

                Binary Subtraction t9 _ ->
                    pushNestedContext rest t9 (GoLeft :: pathR) ctx

                Binary Multiplication t10 _ ->
                    pushNestedContext rest t10 (GoLeft :: pathR) ctx

                Binary Division t11 _ ->
                    pushNestedContext rest t11 (GoLeft :: pathR) ctx

                Unary Square t12 ->
                    pushNestedContext rest t12 (GoLeft :: pathR) ctx

                Unary _ _ ->
                    Debug.crash "impossible"

                Atom _ ->
                    Debug.crash "impossible"

        GoRight :: rest1 ->
            case term of
                Binary Conjunction t14 t13 ->
                    pushNestedContext rest1 t13 (GoRight :: pathR) (pushContextRight (GoLeft :: pathR) t14 ctx)

                Binary Disjunction _ t15 ->
                    pushNestedContext rest1 t15 (GoRight :: pathR) ctx

                Binary Equivalence _ t16 ->
                    pushNestedContext rest1 t16 (GoRight :: pathR) ctx

                Binary Implication t18 t17 ->
                    pushNestedContext rest1 t17 (GoRight :: pathR) (pushContextRight (GoLeft :: pathR) t18 ctx)

                Binary EqualTo _ t19 ->
                    pushNestedContext rest1 t19 (GoRight :: pathR) ctx

                Binary NotEqualTo _ t20 ->
                    pushNestedContext rest1 t20 (GoRight :: pathR) ctx

                Binary Addition _ t21 ->
                    pushNestedContext rest1 t21 (GoRight :: pathR) ctx

                Binary Subtraction _ t22 ->
                    pushNestedContext rest1 t22 (GoRight :: pathR) ctx

                Binary Multiplication _ t23 ->
                    pushNestedContext rest1 t23 (GoRight :: pathR) ctx

                Binary Division _ t24 ->
                    pushNestedContext rest1 t24 (GoRight :: pathR) ctx

                Unary Not t25 ->
                    pushNestedContext rest1 t25 (GoRight :: pathR) ctx

                Unary Neg t26 ->
                    pushNestedContext rest1 t26 (GoRight :: pathR) ctx

                Unary _ _ ->
                    Debug.crash "todo"

                Atom _ ->
                    Debug.crash "impossible"


currentTerm : Direction -> Problem -> Model -> Term
currentTerm direction problem model =
    case direction of
        History ->
            case List.drop model.historyIndex problem.history of
                [] ->
                    problem.start

                t :: _ ->
                    t.term

        Future ->
            case List.drop model.futureIndex problem.future of
                [] ->
                    problem.finish

                t :: _ ->
                    t.term


flippedPath : Model -> Path
flippedPath model =
    case model.direction of
        History ->
            model.futurePath

        Future ->
            model.historyPath


canFocusRight : Model -> Bool
canFocusRight model =
    case model.focus of
        Binary _ _ _ ->
            True

        Unary Square _ ->
            False

        Unary _ _ ->
            True

        Atom _ ->
            False


canFocusOut : Model -> Bool
canFocusOut model =
    case model.path of
        [] ->
            False

        _ :: _ ->
            True


canFocusLeft : Model -> Bool
canFocusLeft model =
    case model.focus of
        Binary _ _ _ ->
            True

        Unary Square _ ->
            True

        Unary _ _ ->
            False

        Atom _ ->
            False


viewRulesPanel : { a | rules : List Rule.Rule } -> Model -> Html msg
viewRulesPanel problem model =
    let
        f idx rule =
            if rule.name == "Arithmetic" then
                li
                    [ class "rule-item" ]
                    [ div [ class "rule-name" ] [ Html.em [] [ text rule.name ] ], Render.leftRuleDivDots ]
            else
                li [ class "rule-item" ] [ div [ class "rule-name" ] [ text rule.name ], Render.leftRuleDiv rule ]
    in
    ul
        [ id "scrolling", class "landscape-hide bottom-list", modelAriaHidden model ]
        (List.indexedMap f problem.rules)


viewNotesPanel : Problem -> Model -> Html Msg
viewNotesPanel problem model =
    div [ id "scrolling", class "landscape-hide notes-panel-body", modelAriaHidden model ]
        ([ Html.p [] [ Html.em [] [ Html.text problem.description ] ] ]
            ++ List.map castHtml problem.notes
        )


panelTitle : Model -> String
panelTitle model =
    case model.panel of
        NotesPanel ->
            "Notes"

        ContextPanel ->
            "Context"

        ScratchPanel ->
            "Scratch Pad"

        TransformationsPanel ->
            "Transformations"

        HistoryPanel ->
            case model.direction of
                History ->
                    "History"

                Future ->
                    "Future"

        RulesPanel ->
            "Rules"


canRedo : Problem -> Model -> Bool
canRedo problem model =
    case model.direction of
        History ->
            model.historyIndex > 0

        Future ->
            model.futureIndex > 0


canUndo : Problem -> Model -> Bool
canUndo problem model =
    case model.direction of
        History ->
            model.historyIndex < List.length problem.history

        Future ->
            model.futureIndex < List.length problem.future


focusColor : Bool -> String
focusColor b =
    if b then
        "White"
    else
        "SlateGrey"


metaClass : Bool -> String
metaClass b =
    if b then
        " meta"
    else
        ""


selectedClass : Bool -> String
selectedClass b =
    if b then
        " selected"
    else
        ""


goalIsHiddenClass : Model -> String
goalIsHiddenClass model =
    if model.goalIsHidden then
        " gone"
    else
        ""


contextSelectionPath : Model -> Path
contextSelectionPath model =
    case model.contextSelection of
        Nothing ->
            []

        Just idx ->
            (unsafeGet idx model.context).path


viewHistoryPanel : { d | finish : Term , future : List { a | isReversed : Bool, rule : Int, term : Term } , history : List { b | isReversed : Bool, rule : Int, term : Term } , rules : List { c | name : String } , start : Term } -> Model -> Html Msg
viewHistoryPanel problem model =
    let
        f len idx1 idx2 step =
            li
                [ class ("history-item" ++ selectedClass (idx1 == idx2)), onClick (HistoryItemMsg idx2) ]
                [ div
                      [ class "history-text" ]
                      [ span [ class "history-gutter" ] [ text (toString (len - idx2)) ]
                      , text
                            ((unsafeGet step.rule problem.rules).name
                                 ++ (if step.isReversed then
                                         " (reverse)"
                                     else
                                         ""
                                    )
                            )
                      ]
                , div [ class "history-formula" ] [ Render.leftTermDiv step.term ]
                ]
    in
    case model.direction of
        History ->
            ol
                [ id "scrolling", class "landscape-hide bottom-list", modelAriaHidden model ]
                (li
                     [ class
                           ("history-item"
                                ++ (if model.historyIndex == List.length problem.history then
                                        " selected"
                                    else
                                        ""
                                   )
                           )
                     , onClick (HistoryItemMsg (List.length problem.history))
                     ]
                     [ div [ class "history-text" ] [ span [ class "history-gutter" ] [ text "0" ], text "Given" ]
                     , div [ class "history-formula" ] [ Render.leftTermDiv problem.start ]
                     ]
                     :: List.reverse (List.indexedMap (f (List.length problem.history) model.historyIndex) problem.history)
                )

        Future ->
            ol
                [ id "scrolling", class "landscape-hide bottom-list", modelAriaHidden model ]
                (List.indexedMap (f (negate (List.length problem.future)) model.futureIndex) problem.future
                     ++ [ li
                              [ class ("history-item" ++ selectedClass (model.futureIndex == List.length problem.future))
                              , onClick (HistoryItemMsg (List.length problem.future))
                              ]
                              [ div [ class "history-gutter" ] [ text "End" ], div [] [ Render.leftTermDiv problem.finish ] ]
                        ]
                )


viewTransformationsPanel : { a | rules : List Rule.Rule } -> Model -> Html Msg
viewTransformationsPanel problem model =
    let
        f idx sub =
            let
                rule =
                    unsafeGet sub.ruleIndex problem.rules
            in
            if rule.name == "Arithmetic" then
                li
                    [ class ("rule-item" ++ selectedClass (model.transformationSelection == Just idx))
                    , onClick (TransformationItemMsg idx)
                    ]
                    [ div
                          [ class "rule-name" ]
                          [ Html.em [] [ text rule.name ]
                          , if sub.isReversed then
                                span [ class "flipped" ] [ text "reversed" ]
                            else
                                text ""
                          ]
                    , Render.leftRuleDivDots
                    ]
            else
                li
                    [ class ("rule-item" ++ selectedClass (model.transformationSelection == Just idx))
                    , onClick (TransformationItemMsg idx)
                    ]
                    [ div
                          [ class "rule-name" ]
                          [ text rule.name
                          , if sub.isReversed then
                                span [ class "flipped" ] [ text "reversed" ]
                            else
                                text ""
                          ]
                    , Render.leftRuleDiv (Rule.reverseIf sub.isReversed rule)
                    ]
    in
    ul
        [ id "scrolling", class "landscape-hide bottom-list", modelAriaHidden model ]
        (List.indexedMap f model.transformations)


viewScratchPanel : { a | scratch : List Term } -> Model -> Html Msg
viewScratchPanel problem model =
    let
        f idx term =
            li
                [ class ("term-item" ++ selectedClass (model.scratchSelection == Just idx)) ]
                [ div [ class "term-item-formula", onClick (ScratchItemMsg idx) ] [ Render.leftTermDiv term ]
                , if model.scratchSelection == Just idx && not (isReservedAtom term) then
                      button [ class "scratch-remove-button", title "Delete", onClick ScratchRemoveMsg ] [ Graphics.delete ]
                  else
                      text ""
                ]
    in
    ol
        [ id "scrolling", class "landscape-hide bottom-list", modelAriaHidden model ]
        (List.indexedMap f problem.scratch)


viewTitleButtons : Problem -> Model -> List (Html Msg)
viewTitleButtons problem model =
    case model.panel of
        ScratchPanel ->
            [ button [ class "panel-button", onClick NumberAddMsg ] [ text "Number" ]
            , button [ class "panel-button", onClick ScratchAddMsg ] [ text "Combine" ]
            ]

        TransformationsPanel ->
            [ button
                  [ class "panel-button", disabled (not (canUndo problem model)), onClick TransformationUndoMsg ]
                  [ text "Undo" ]
            , button
                  [ class "panel-button", disabled (not (canRedo problem model)), onClick TransformationRedoMsg ]
                  [ text "Redo" ]
            ]

        _ ->
            []


viewContextPanel : a -> Model -> Html Msg
viewContextPanel problem model =
    let
        f idx x =
            li
                [ class ("term-item" ++ selectedClass (model.contextSelection == Just idx))
                , onClick (ContextItemMsg idx)
                ]
                (List.singleton
                     ((if model.contextSelection == Just idx then
                           Render.yellowLeftTermDiv
                       else
                           Render.leftTermDiv
                      )
                          x.term
                     )
                )
    in
    ol
        [ id "scrolling", class "landscape-hide bottom-list", modelAriaHidden model ]
        (li [ class "item-text" ] [ text "Basic arithmetic" ]
             :: li [ class "item-text" ] [ text "Basic relations" ] :: List.indexedMap f model.context
        )


viewPanelBody : Problem -> Model -> Html Msg
viewPanelBody problem model =
    case model.panel of
        NotesPanel ->
            viewNotesPanel problem model

        ContextPanel ->
            viewContextPanel problem model

        ScratchPanel ->
            viewScratchPanel problem model

        TransformationsPanel ->
            viewTransformationsPanel problem model

        HistoryPanel ->
            viewHistoryPanel problem model

        RulesPanel ->
            viewRulesPanel problem model


resetPop : Model -> Html Msg
resetPop model =
    div [ class "popup reset-popup" ]
        [ div [ class "reset-popup-text" ] [ text "Are you sure you want to reset this problem?" ]
        , div [ class "reset-popup-buttons" ]
            [ button [ class "panel-button", onClick ResetPopUpCancelMsg ] [ text "Cancel" ]
            , button [ class "panel-button", onClick ResetPopUpResetMsg ] [ text "Reset" ]
            ]
        ]


copyPop : Model -> Html Msg
copyPop model =
    div [ class "popup copy-popup" ]
        [ div [ class "copy-popup-text" ] [ text "Copied to clipboard." ]
        , div [ class "copy-popup-buttons" ]
            [ button [ class "panel-button", onClick ResetPopUpCancelMsg ] [ text "OK" ]
            ]
        ]


settingsPop : { b | proof : Maybe a } -> c -> Html Msg
settingsPop problem model =
    div
        [ class "popup settings-popup" ]
        [ ul
              []
              [ li
                    [ class "panel-select-item" ]
                    [ button
                          [ class "panel-select-button", disabled (isNothing problem.proof), onClick SettingsShowProofMsg ]
                          [ text "Show proof…" ]
                    ]
              , li
                    [ class "panel-select-item" ]
                    [ button [ class "panel-select-button", onClick SettingsResetMsg ] [ text "Reset problem" ] ]
              ]
        ]


viewNavigationButtons : Model -> List (Html Msg)
viewNavigationButtons model =
    if isNothing model.transformationSelection then
        [ div [ class "time-arrow-size" ] []
        , button [ class "fab", title "In Left", onClick FocusLeftMsg, disabled (not <| canFocusLeft model) ]
            [ Graphics.circle (focusColor (canFocusLeft model)) ]
        , button [ class "fab", title "Out", onClick FocusOutMsg, disabled (not <| canFocusOut model) ]
            [ Graphics.ring (focusColor (canFocusOut model)) ]
        , button [ class "fab", title "In Right", onClick FocusRightMsg, disabled (not <| canFocusRight model) ]
            [ Graphics.circle (focusColor (canFocusRight model)) ]
        , case model.direction of
            History ->
                button [ class "time-arrow-size", title "Direction Forwards", onClick ChangeDirectionMsg ]
                    [ Graphics.arrow ]

            Future ->
                button [ class "time-arrow-size", title "Direction Backwards", onClick ChangeDirectionMsg ]
                    [ Graphics.target ]
        ]
    else
        [ button
            [ class ("basic" ++ metaClass (transformationIsMultiple model))
            , onClick TransformationUseMsg
            ]
            [ text "Apply" ]
        ]


navBar : Model -> Html Msg
navBar model =
    div [ class "landscape-hide nav-bar", modelAriaHidden model ]
        [ if model.solved then
            button [ class "nav-bar-button", title "Q.E.D.", onClick SolvedMsg ] [ Graphics.whiteTombstone ]
          else
            button [ class "nav-bar-button", title "Back", onClick BackMsg ] [ Graphics.leftAngle ]
        , div [ class "top-title" ] [ text <| "Problem " ++ toString (model.problemIndex + 1) ]
        , button [ class "nav-bar-button", title "Menu", onClick ShowSettingsMsg ] [ Graphics.dots3 ]
        ]


scrollingCmd : Model -> Cmd Msg
scrollingCmd model =
    case model.dialog of
        NoDialog ->
            case model.panel of
                NotesPanel ->
                    scrollToTopCmd IgnoreMsg

                ContextPanel ->
                    scrollToTopCmd IgnoreMsg

                ScratchPanel ->
                    scrollToTopCmd IgnoreMsg

                TransformationsPanel ->
                    scrollToTopCmd IgnoreMsg

                HistoryPanel ->
                    case model.direction of
                        History ->
                            scrollToBottomCmd IgnoreMsg

                        Future ->
                            scrollToTopCmd IgnoreMsg

                RulesPanel ->
                    scrollToTopCmd IgnoreMsg

        TransformationDialog _ ->
            scrollToTopCmd IgnoreMsg

        NumberDialog _ ->
            scrollToTopCmd IgnoreMsg

        ScratchDialog _ ->
            scrollToTopCmd IgnoreMsg


transformedTerm : Model -> Term
transformedTerm model =
    if isNothing model.transformationSelection || model.panel /= TransformationsPanel then
        model.term
    else
        termReplace model.path (currentTransformation model).display model.term


pop : a -> Html Msg
pop model =
    div
        [ class "popup panel-popup" ]
        [ ul
              []
              [ li
                    [ class "panel-select-item" ]
                    [ button [ class "panel-select-button", onClick (PanelSelectChoiceMsg NotesPanel) ] [ text "Notes" ]
                    ]
              , li
                    [ class "panel-select-item" ]
                    [ button
                          [ class "panel-select-button", onClick (PanelSelectChoiceMsg TransformationsPanel) ]
                          [ text "Transformations" ]
                    ]
              , li
                    [ class "panel-select-item" ]
                    [ button [ class "panel-select-button", onClick (PanelSelectChoiceMsg HistoryPanel) ] [ text "History" ]
                    ]
              , li
                    [ class "panel-select-item" ]
                    [ button [ class "panel-select-button", onClick (PanelSelectChoiceMsg ScratchPanel) ] [ text "Scratch Pad" ]
                    ]
              , li
                    [ class "panel-select-item" ]
                    [ button [ class "panel-select-button", onClick (PanelSelectChoiceMsg RulesPanel) ] [ text "Rules" ]
                    ]
              ]
        ]


viewNoDialog : Problem -> Model -> Html Msg
viewNoDialog problem model =
    div
        [ class "content-body" ]
        [ navBar model
        , div
              [ class "main-formula", modelAriaHidden model ]
              [ Render.formulaHtml2 (transformedTerm model) model.path (contextSelectionPath model) ]
        , div [ class "navigation", modelAriaHidden model ] (viewNavigationButtons model)
        , div
              [ class ("goal-formula" ++ goalIsHiddenClass model), modelAriaHidden model ]
              [ Render.formulaHtml2 problem.finish [] [] ]
        , div
              [ class "panel-title landscape-hide", modelAriaHidden model ]
              ([ button
                     [ class "panel-title-button", onClick ShowPanelSelectMsg ]
                     [ div [ class "up-down-arrows" ] [ Graphics.upDownArrows ]
                     , div [ class "panel-title-text" ] [ text (panelTitle model) ]
                     ]
               ]
                   ++ [ div [ class "grow" ] [] ] ++ viewTitleButtons problem model
              )
        , viewPanelBody problem model
        , case model.popUp of
              NoPopUp ->
                  text ""

              _ ->
                  div [ class "popup-background", title "Close Pop-Up", onClick PanelSelectBackgroundMsg ] []
        , case model.popUp of
              NoPopUp ->
                  text ""

              PanelSelectPopUp ->
                  pop model

              SettingsPopUp ->
                  settingsPop problem model

              ResetPopUp ->
                  resetPop model

              CopyPopUp ->
                  copyPop model
        ]


view : Problem -> Model -> Html Msg
view problem model =
    case model.dialog of
        NoDialog ->
            viewNoDialog problem model

        TransformationDialog model1 ->
            Html.map TransformationDialogMsg (Dialog.Transformation.view model1)

        NumberDialog model2 ->
            Html.map NumberDialogMsg (Dialog.Number.view model2)

        ScratchDialog model3 ->
            Html.map ScratchDialogMsg (Dialog.Scratch.view model3)


reallyInit : b -> { context : List c , contextSelection : Maybe a , dialog : Dialog , direction : Direction , focus : Term , futureIndex : number , futurePath : List d , goalIsHidden : Bool , historyIndex : number1 , historyPath : List e , panel : Panel , path : List f , popUp : PopUp , problemIndex : b , scratchSelection : Maybe a1 , solved : Bool , term : Term , transformationSelection : Maybe a2 , transformations : List g }
reallyInit idx =
    { problemIndex = idx
    , direction = History
    , historyIndex = 0
    , futureIndex = 0
    , historyPath = []
    , futurePath = []
    , dialog = NoDialog
    , panel = NotesPanel
    , popUp = NoPopUp
    , contextSelection = Nothing
    , scratchSelection = Nothing
    , transformationSelection = Nothing
    , goalIsHidden = False
    , term = Atom Top
    , focus = Atom Top
    , context = []
    , path = []
    , transformations = []
    , solved = False
    }


flipDirection : Direction -> Direction
flipDirection d =
    case d of
        History ->
            Future

        Future ->
            History


refocus : Path -> Problem -> Model -> Model
refocus path problem model =
    let
        ( historyPath, futurePath ) =
            case model.direction of
                History ->
                    ( path, model.futurePath )

                Future ->
                    ( model.historyPath, path )

        term =
            currentTerm model.direction problem model

        focus =
            termFocus path term

        context =
            List.reverse (pushNestedContext path term [] [])

        transformations =
            Transformations.transformations problem.rules problem.scratch (List.map .term context) focus

        solved =
            currentTerm History problem model == currentTerm Future problem model
    in
        { model
            | historyPath = historyPath
            , futurePath = futurePath
            , term = term
            , focus = focus
            , context = context
            , path = path
            , transformations = transformations
            , solved = solved
            , scratchSelection = Nothing
            , contextSelection = Nothing
            , transformationSelection = Nothing
        }


init : Int -> Problem -> Model
init idx problem =
    refocus [] problem (reallyInit idx)


update : Problem -> Msg -> Model -> OutMsg
update problem msg model =
    case msg of
        IgnoreMsg ->
            UnchangedOutMsg

        BackMsg ->
            ExitOutMsg

        SolvedMsg ->
            SolvedOutMsg (proofSteps problem model) { problem | solved = True }

        FocusLeftMsg ->
            ModelChangedOutMsg (refocus (Path.left model.path) problem model)

        FocusOutMsg ->
            ModelChangedOutMsg (refocus (Path.out model.path) problem model)

        FocusRightMsg ->
            ModelChangedOutMsg (refocus (Path.right model.path) problem model)

        ShowPanelSelectMsg ->
            ModelChangedOutMsg { model | popUp = PanelSelectPopUp }

        PanelSelectBackgroundMsg ->
            ModelChangedOutMsg { model | popUp = NoPopUp }

        PanelSelectChoiceMsg panel ->
            if panel == model.panel then
                ModelChangedOutMsg { model | popUp = NoPopUp }
            else
                let
                    newModel =
                        refocus model.path problem { model | panel = panel, popUp = NoPopUp }
                in
                ModelCmdChangedOutMsg newModel (scrollingCmd newModel)

        ShowSettingsMsg ->
            ModelChangedOutMsg { model | popUp = SettingsPopUp }

        SettingsBackgroundMsg ->
            ModelChangedOutMsg { model | popUp = NoPopUp }

        SettingsEmailMsg ->
            ModelCmdChangedOutMsg { model | popUp = NoPopUp } (Ports.email ())

        SettingsCopyMsg ->
            ModelCmdChangedOutMsg { model | popUp = CopyPopUp } (Ports.copy (tex (transformedTerm model)))

        SettingsResetMsg ->
            ModelChangedOutMsg { model | popUp = ResetPopUp }

        ResetPopUpCancelMsg ->
            ModelChangedOutMsg { model | popUp = NoPopUp }

        ResetPopUpResetMsg ->
            let
                newModel =
                    init model.problemIndex newProblem

                newProblem =
                    Problem.reset problem
            in
            ProblemChangedOutMsg newProblem newModel

        ScratchCopyMsg ->
            let
                newProblem =
                    { problem
                        | scratch = List.filter (\x -> not (x == model.focus)) problem.scratch ++ [ model.focus ]
                        }
            in
            ProblemChangedOutMsg newProblem (refocus model.path newProblem model)

        ScratchAddMsg ->
            ModelCmdChangedOutMsg
                { model
                    | dialog = ScratchDialog (Dialog.Scratch.init problem.scratchSymbols problem.scratch model.focus)
                    }
                (scrollToTopCmd IgnoreMsg)

        NumberAddMsg ->
            ModelChangedOutMsg { model | dialog = NumberDialog Dialog.Number.init }

        ScratchRemoveMsg ->
            let
                newProblem =
                    { problem | scratch = remove (fromJust model.scratchSelection) problem.scratch }
            in
            ProblemChangedOutMsg newProblem (refocus model.path newProblem model)

        NumberDialogMsg msg1 ->
            case model.dialog of
                NumberDialog model1 ->
                    case Dialog.Number.update msg1 model1 of
                        Dialog.Number.NoChangeOut ->
                            UnchangedOutMsg

                        Dialog.Number.ChangedOut model2 ->
                            ModelChangedOutMsg { model | dialog = NumberDialog model2 }

                        Dialog.Number.ExitOut ->
                            ModelChangedOutMsg { model | dialog = NoDialog }

                        Dialog.Number.AddOut t1 ->
                            let
                                newProblem =
                                    { problem | scratch = List.filter (\t -> not (t == t1)) problem.scratch ++ [ t1 ] }

                                newModel =
                                    { model | dialog = NoDialog }
                            in
                            ProblemChangedOutMsg newProblem (refocus model.path newProblem newModel)

                _ ->
                    Debug.crash "impossible"

        ScratchDialogMsg msg2 ->
            case model.dialog of
                ScratchDialog model3 ->
                    case Dialog.Scratch.update msg2 model3 of
                        Dialog.Scratch.NoChangeOut ->
                            UnchangedOutMsg

                        Dialog.Scratch.ChangedOut model4 ->
                            ModelChangedOutMsg { model | dialog = ScratchDialog model4 }

                        Dialog.Scratch.ChangedCmdOut model5 x1 ->
                            ModelCmdChangedOutMsg { model | dialog = ScratchDialog model5 } (Cmd.map ScratchDialogMsg x1)

                        Dialog.Scratch.ExitOut ->
                            ModelChangedOutMsg { model | dialog = NoDialog }

                        Dialog.Scratch.AddOut t2 ->
                            let
                                newProblem =
                                    { problem | scratch = List.filter (\t -> not (t == t2)) problem.scratch ++ [ t2 ] }

                                newModel =
                                    { model | dialog = NoDialog }
                            in
                            ProblemChangedOutMsg newProblem (refocus model.path newProblem newModel)

                _ ->
                    Debug.crash "impossible"

        TransformationDialogMsg msg3 ->
            case model.dialog of
                TransformationDialog model6 ->
                    case Dialog.Transformation.update msg3 model6 of
                        Dialog.Transformation.EmptyOut ->
                            UnchangedOutMsg

                        Dialog.Transformation.ChangedOut model7 ->
                            ModelChangedOutMsg { model | dialog = TransformationDialog model7 }

                        Dialog.Transformation.CancelOut ->
                            ModelChangedOutMsg { model | dialog = NoDialog, transformationSelection = Nothing }

                        Dialog.Transformation.OkOut transformation t3 ->
                            let
                                newModel2 =
                                    refocus model.path newProblem { newModel1 | dialog = NoDialog }

                                newProblem =
                                    case model.direction of
                                        History ->
                                            { problem
                                                | history =
                                                      { term = term
                                                      , rule = transformation.ruleIndex
                                                      , isReversed = transformation.isReversed
                                                      , path = model.path
                                                      }
                                                          :: List.drop model.historyIndex problem.history
                                                }

                                        Future ->
                                            { problem
                                                | future =
                                                      { term = term
                                                      , rule = transformation.ruleIndex
                                                      , isReversed = transformation.isReversed
                                                      , path = model.path
                                                      }
                                                          :: List.drop model.futureIndex problem.future
                                                }

                                term =
                                    termReplace model.path t3 model.term

                                newModel1 =
                                    case model.direction of
                                        History ->
                                            { model | historyIndex = 0 }

                                        Future ->
                                            { model | futureIndex = 0 }
                            in
                            ProblemChangedOutMsg newProblem newModel2

                _ ->
                    Debug.crash "impossible"

        ScratchItemMsg i1 ->
            if model.scratchSelection == Just i1 then
                ModelChangedOutMsg { model | scratchSelection = Nothing }
            else
                ModelChangedOutMsg { model | scratchSelection = Just i1 }

        ContextItemMsg i2 ->
            if model.contextSelection == Just i2 then
                ModelChangedOutMsg { model | contextSelection = Nothing }
            else
                ModelChangedOutMsg { model | contextSelection = Just i2 }

        TransformationItemMsg i3 ->
            let
                rule =
                    unsafeGet sub.ruleIndex problem.rules

                sub =
                    unsafeGet i3 model.transformations
            in
            if sub.isMultiple then
                ModelCmdChangedOutMsg
                    { model
                        | dialog =
                              TransformationDialog
                                  (Dialog.Transformation.init
                                       model.focus
                                       (List.map .term model.context)
                                       problem.scratch
                                       (Rule.reverseIf sub.isReversed rule)
                                       sub
                                  )
                        }
                    (scrollToTopCmd IgnoreMsg)
            else
                let
                    newModel2 =
                        refocus model.path newProblem newModel1

                    newProblem =
                        case model.direction of
                            History ->
                                { problem
                                    | history =
                                          { term = term, rule = sub.ruleIndex, isReversed = sub.isReversed, path = model.path }
                                              :: List.drop model.historyIndex problem.history
                                    }

                            Future ->
                                { problem
                                    | future =
                                          { term = term, rule = sub.ruleIndex, isReversed = sub.isReversed, path = model.path }
                                              :: List.drop model.futureIndex problem.future
                                    }

                    term =
                        termReplace model.path sub.display model.term

                    newModel1 =
                        case model.direction of
                            History ->
                                { model | historyIndex = 0 }

                            Future ->
                                { model | futureIndex = 0 }
                in
                ProblemChangedOutMsg newProblem newModel2

        TransformationUseMsg ->
            let
                rule =
                    unsafeGet sub.ruleIndex problem.rules

                sub =
                    currentTransformation model
            in
            if sub.isMultiple then
                ModelCmdChangedOutMsg
                    { model
                        | dialog =
                              TransformationDialog
                                  (Dialog.Transformation.init
                                       model.focus
                                       (List.map .term model.context)
                                       problem.scratch
                                       (Rule.reverseIf sub.isReversed rule)
                                       sub
                                  )
                        }
                    (scrollToTopCmd IgnoreMsg)
            else
                let
                    newModel2 =
                        refocus model.path newProblem newModel1

                    newProblem =
                        case model.direction of
                            History ->
                                { problem
                                    | history =
                                          { term = term, rule = sub.ruleIndex, isReversed = sub.isReversed, path = model.path }
                                              :: List.drop model.historyIndex problem.history
                                    }

                            Future ->
                                { problem
                                    | future =
                                          { term = term, rule = sub.ruleIndex, isReversed = sub.isReversed, path = model.path }
                                              :: List.drop model.futureIndex problem.future
                                    }

                    term =
                        termReplace model.path sub.display model.term

                    newModel1 =
                        case model.direction of
                            History ->
                                { model | historyIndex = 0 }

                            Future ->
                                { model | futureIndex = 0 }
                in
                ProblemChangedOutMsg newProblem newModel2

        ChangeDirectionMsg ->
            ModelChangedOutMsg { model | goalIsHidden = not model.goalIsHidden }

        HistoryItemMsg i4 ->
            case model.direction of
                History ->
                    if i4 == model.historyIndex then
                        UnchangedOutMsg
                    else
                        ModelChangedOutMsg (refocus [] problem { model | historyIndex = i4 })

                Future ->
                    if i4 == model.futureIndex then
                        UnchangedOutMsg
                    else
                        ModelChangedOutMsg (refocus [] problem { model | futureIndex = i4 })

        SettingsShowProofMsg ->
            ShowProofOutMsg

        TransformationUndoMsg ->
            case model.direction of
                History ->
                    ModelChangedOutMsg
                        (refocus
                             (unsafeGet model.historyIndex problem.history).path
                             problem
                             { model | historyIndex = model.historyIndex + 1 }
                        )

                Future ->
                    ModelChangedOutMsg
                        (refocus
                             (unsafeGet model.futureIndex problem.future).path
                             problem
                             { model | futureIndex = model.futureIndex + 1 }
                        )

        TransformationRedoMsg ->
            case model.direction of
                History ->
                    ModelChangedOutMsg
                        (refocus
                             (unsafeGet (model.historyIndex - 1) problem.history).path
                             problem
                             { model | historyIndex = model.historyIndex - 1 }
                        )

                Future ->
                    ModelChangedOutMsg
                        (refocus
                             (unsafeGet (model.futureIndex - 1) problem.future).path
                             problem
                             { model | futureIndex = model.futureIndex - 1 }
                        )
