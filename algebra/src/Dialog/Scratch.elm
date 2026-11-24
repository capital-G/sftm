module Dialog.Scratch exposing (..)

import Term exposing (Term)
import Html exposing (Html, div, span, text, button, ul, li)
import Html.Attributes exposing (id, class, disabled)
import Html.Events exposing (onClick)
import Graphics
import Utilities exposing (unsafeGet, castHtml, scrollToTopCmd)
import Render
import Svg
import Svg.Attributes as SA
import Task
import Dom.Scroll


type alias Model =
    { selection : Selection
    , symbol : Maybe Int
    , symbols : List Term
    , scratch : List Term
    , focus : Term
    , logA : Maybe Int
    , logB : Maybe Int
    , algA : Maybe Int
    , algB : Maybe Int
    }


type Selection
    = LeftSelection
    | SymbolSelection
    | RightSelection


type Msg
    = IgnoreMsg
    | ListItemClickedMsg Int
    | OkClickedMsg
    | CancelClickedMsg
    | SelectionClickedMsg Selection


type Out
    = NoChangeOut
    | ChangedOut Model
    | ChangedCmdOut Model (Cmd Msg)
    | ExitOut
    | AddOut Term


isSelected : Maybe Int -> Int -> Bool
isSelected selection i1 =
    case selection of
        Nothing ->
            False

        Just i2 ->
            i1 == i2


toggle : Int -> Maybe Int -> Maybe Int
toggle i m =
    if m == Just i then
        Nothing
    else
        Just i


hiddenClass b =
    if b then
        " hidden"
    else
        ""


selectedClass : Bool -> String
selectedClass b =
    if b then
        " selected"
    else
        ""


isHiddenRight : Model -> Bool
isHiddenRight model =
    case Maybe.map (\i -> unsafeGet i model.symbols) model.symbol of
        Nothing ->
            True

        Just x1 ->
            case x1 of
                Term.Binary _ _ _ ->
                    False

                Term.Unary Term.Square _ ->
                    True

                Term.Unary _ _ ->
                    False

                Term.Atom _ ->
                    True


isHiddenLeft : Model -> Bool
isHiddenLeft model =
    case Maybe.map (\i -> unsafeGet i model.symbols) model.symbol of
        Nothing ->
            True

        Just x1 ->
            case x1 of
                Term.Unary Term.Square _ ->
                    False

                Term.Unary _ _ ->
                    True

                _ ->
                    isHiddenRight model


getSymbol : Model -> Term
getSymbol model =
    case Maybe.map (\i -> unsafeGet i model.symbols) model.symbol of
        Nothing ->
            Term.Atom Term.Top

        Just x1 ->
            x1


isUnarySelection : Model -> Bool
isUnarySelection model =
    case Maybe.map (\i -> unsafeGet i model.symbols) model.symbol of
        Just (Term.Unary _ _) ->
            True

        _ ->
            False


isAlgebraicSelection : Model -> Bool
isAlgebraicSelection model =
    case Maybe.map (\i -> unsafeGet i model.symbols) model.symbol of
        Just x1 ->
            Term.hasAlgebraicSubterms x1

        Nothing ->
            False


filteredScratch : Model -> List Term
filteredScratch model =
    if isAlgebraicSelection model then
        List.filter Term.isAlgebraic model.scratch
    else
        List.filter Term.isLogical model.scratch


formula : Model -> Term
formula model =
    let
        algA =
            Maybe.map (\i -> unsafeGet i (filteredScratch model)) model.algA

        symbol =
            Maybe.map (\i -> unsafeGet i model.symbols) model.symbol

        algB =
            Maybe.map (\i -> unsafeGet i (filteredScratch model)) model.algB
    in
    case symbol of
        Nothing ->
            Debug.crash "impossible"

        Just x1 ->
            case x1 of
                Term.Atom _ ->
                    x1

                Term.Unary op t1 ->
                    Term.Unary op (Maybe.withDefault t1 algA)

                Term.Binary op1 t2 t3 ->
                    Term.Binary op1 (Maybe.withDefault t2 algA) (Maybe.withDefault t3 algB)


isValid : Model -> Bool
isValid model =
    let
        algA =
            Maybe.map (\i -> unsafeGet i (filteredScratch model)) model.algA

        symbol =
            Maybe.map (\i -> unsafeGet i model.symbols) model.symbol

        algB =
            Maybe.map (\i -> unsafeGet i (filteredScratch model)) model.algB
    in
    case symbol of
        Nothing ->
            False

        Just x1 ->
            case x1 of
                Term.Atom _ ->
                    True

                Term.Unary _ _ ->
                    case algA of
                        Nothing ->
                            False

                        Just _ ->
                            True

                Term.Binary _ _ _ ->
                    case ( algA, algB ) of
                        ( (Just _), (Just _) ) ->
                            True

                        ( _, _ ) ->
                            False


init : b -> c -> d -> { algA : Maybe a , algB : Maybe a1 , focus : d , logA : Maybe a2 , logB : Maybe a3 , scratch : c , selection : Selection , symbol : Maybe a4 , symbols : b }
init symbols scratch focus =
    { selection = SymbolSelection
    , symbol = Nothing
    , symbols = symbols
    , scratch = scratch
    , focus = focus
    , logA = Nothing
    , logB = Nothing
    , algA = Nothing
    , algB = Nothing
    }


leftButtonView : Model -> Html Msg
leftButtonView model =
    button
        [ class
              ("circle-button-2" ++ selectedClass (model.selection == LeftSelection) ++ hiddenClass (isHiddenLeft model))
        , onClick (SelectionClickedMsg LeftSelection)
        ]
        [ case getSymbol model of
              Term.Unary Term.Square t1 ->
                  Render.centerTermDiv t1

              Term.Binary _ t2 _ ->
                  Render.centerTermDiv t2

              _ ->
                  Render.centerTermDiv (Term.Atom (Term.MetaVar Term.MetaA))
        ]


symbolButtonView : Model -> Html Msg
symbolButtonView model =
    button
        [ class ("circle-button-2" ++ selectedClass (model.selection == SymbolSelection))
        , onClick <| SelectionClickedMsg SymbolSelection
        ]
        [ Render.starDiv
        ]


rightButtonView : Model -> Html Msg
rightButtonView model =
    button
        [ class
              ("circle-button-2" ++ selectedClass (model.selection == RightSelection) ++ hiddenClass (isHiddenRight model))
        , onClick (SelectionClickedMsg RightSelection)
        ]
        [ case getSymbol model of
              Term.Unary _ t1 ->
                  Render.centerTermDiv t1

              Term.Binary _ _ t2 ->
                  Render.centerTermDiv t2

              Term.Atom _ ->
                  Render.centerTermDiv (Term.Atom (Term.MetaVar Term.MetaB))
        ]


itemView : (Int -> Bool) -> Int -> Html Msg -> Html Msg
itemView isSelected idx svg =
    li
        [ class ("term-item" ++ selectedClass (isSelected idx))
        , onClick (ListItemClickedMsg idx)
        ]
        [ svg ]


symbolListView : Maybe Int -> List Term -> Html Msg
symbolListView selection symbols =
    ul [] (List.indexedMap (itemView (isSelected selection)) (List.map Render.termSpan symbols))


scratchListView : Maybe Int -> List Term -> Html Msg
scratchListView selection scratch =
    scratch
        |> List.map Render.leftTermDiv
        |> List.indexedMap (itemView (isSelected selection))
        |> ul []


listView : Model -> Html Msg
listView model =
    case model.selection of
        LeftSelection ->
            if isAlgebraicSelection model then
                scratchListView model.algA (filteredScratch model)
            else
                scratchListView model.logA (filteredScratch model)

        SymbolSelection ->
            symbolListView model.symbol model.symbols

        RightSelection ->
            if isAlgebraicSelection model then
                if isUnarySelection model then
                    scratchListView model.algA (filteredScratch model)
                else
                    scratchListView model.algB (filteredScratch model)
            else if isUnarySelection model then
                scratchListView model.logA (filteredScratch model)
            else
                scratchListView model.logB (filteredScratch model)


view : Model -> Html Msg
view model =
    div
        [ class "content-body" ]
        [ div [ class "nav-bar" ] [ div [] [], div [ class "top-title" ] [ text "Scratch Pad Combine" ], div [] [] ]
        , div
              [ class "main-formula" ]
              (if Utilities.isNothing model.symbol then
                   []
               else
                   [ Render.formulaHtml0 (formula model) ]
              )
        , div
              [ class "dialog-control" ]
              [ button [ class "basic", onClick CancelClickedMsg ] [ text "Cancel" ]
              , div [ Html.Attributes.style [ ( "width", "8px" ) ] ] []
              , button [ class "basic", disabled (not (isValid model)), onClick OkClickedMsg ] [ text "OK" ]
              ]
        , div [ class "dialog-command" ] [ leftButtonView model, symbolButtonView model, rightButtonView model ]
        , div [ id "scrolling", class "panel-body" ] [ listView model ]
        ]


update : Msg -> Model -> Out
update msg model =
    case msg of
        IgnoreMsg ->
            NoChangeOut

        ListItemClickedMsg i1 ->
            case model.selection of
                LeftSelection ->
                    if isAlgebraicSelection model then
                        ChangedOut { model | algA = toggle i1 model.algA }
                    else
                        ChangedOut { model | logA = toggle i1 model.logA }

                SymbolSelection ->
                    ChangedOut { model | symbol = toggle i1 model.symbol }

                RightSelection ->
                    if isAlgebraicSelection model then
                        if isUnarySelection model then
                            ChangedOut { model | algA = toggle i1 model.algA }
                        else
                            ChangedOut { model | algB = toggle i1 model.algB }
                    else if isUnarySelection model then
                        ChangedOut { model | logA = toggle i1 model.logA }
                    else
                        ChangedOut { model | logB = toggle i1 model.logB }

        CancelClickedMsg ->
            ExitOut

        OkClickedMsg ->
            AddOut (formula model)

        SelectionClickedMsg selection ->
            ChangedCmdOut { model | selection = selection } (scrollToTopCmd IgnoreMsg)
