module Dialog.Transformation exposing (..)

import Rule exposing (Rule)
import Term exposing (Term, MetaVar)
import Transformation exposing (Transformation)
import Graphics
import Html exposing (Html, div, span, text, button, ul, li)
import Html.Attributes exposing (id, class, disabled)
import Html.Events exposing (onClick)
import Render
import Svg
import Svg.Attributes as SA
import Transformations
import Utilities exposing (unsafeGet, castHtml, fromJust, isNothing)


type alias Model =
    { display : Term
    , option : List MetaVar
    , options : List Term
    , selectedOption : Int
    , selectedTerms : List (Maybe Term)
    , rule : Rule
    , transformation : Transformation
    , focus : Term
    , context : List Term
    , scratch : List Term
    }


type Msg
    = EmptyMsg
    | SelectionClickedMsg Int
    | ListItemClickedMsg Term
    | CancelClickedMsg
    | OkClickedMsg


type Out
    = EmptyOut
    | ChangedOut Model
    | CancelOut
    | OkOut Transformation Term


updateDisplay : Model -> Model
updateDisplay model =
    let
        dictionary =
            List.map2
                (\v mt ->
                     case mt of
                         Nothing ->
                             ( v, [ Term.Atom (Term.MetaVar v) ] )

                         Just x1 ->
                             ( v, [ x1 ] )
                )
                model.option
                model.selectedTerms
    in
    { model | display = Transformations.substitute dictionary model.transformation.display }


selectedClass b =
    if b then
        " selected"
    else
        ""


init : Term -> List Term -> List Term -> { b | antecedents : List Term , isSymmetric : Bool , left : Term , name : String , right : Term } -> { d | display : c, isReversed : Bool } -> { context : List Term , display : c , focus : Term , option : List MetaVar , options : List Term , rule : { b | antecedents : List Term , isSymmetric : Bool , left : Term , name : String , right : Term } , scratch : List Term , selectedOption : number , selectedTerms : List (Maybe a) , transformation : { d | display : c, isReversed : Bool } }
init focus context scratch rule transformation =
    let
        dict2 =
            .dictionary
                (fromJust
                     (Transformations.antecedents
                          focus
                          scratch
                          context
                          (fromJust
                               (Transformations.performUnification
                                    focus
                                    (Transformations.addDictionary
                                         { rule =
                                               { name = rule.name
                                               , isSymmetric = rule.isSymmetric
                                               , antecedents = rule.antecedents
                                               , left = rule.left
                                               , right = rule.right
                                               , index = 0
                                               }
                                         , direction = direction
                                         }
                                    )
                               )
                          )
                     )
                )

        direction =
            if rule.name == "Arithmetic" && transformation.isReversed then
                Transformations.Leftwards
            else
                Transformations.Rightwards
    in
    case
        List.foldl
            (\( a1, b1 ) ( a2, b2 ) ->
                 if List.length b1 > 1 then
                     ( a1 :: a2, b1 :: b2 )
                 else
                     ( a2, b2 )
            )
            ( [], [] )
            dict2
    of
        ( option, options ) ->
            { display = transformation.display
            , option = List.reverse option
            , options = unsafeGet 0 (List.reverse options)
            , selectedOption = 0
            , selectedTerms = List.map (\x -> Nothing) option
            , rule = rule
            , transformation = transformation
            , focus = focus
            , scratch = scratch
            , context = context
            }


itemView : Maybe Term -> Term -> Html Msg
itemView selection option =
    li
        [ class ("term-item" ++ selectedClass (selection == Just option))
        , onClick <| ListItemClickedMsg option
        ]
        [ option |> Render.leftTermDiv
        ]


listView : Maybe Term -> List Term -> Html Msg
listView selection options =
    ul [] (List.map (itemView selection) options)


view : Model -> Html Msg
view model =
    div
        [ class "content-body" ]
        [ div [ class "nav-bar" ] [ div [] [], div [ class "top-title" ] [ text "Transformation" ], div [] [] ]
        , div [ class "main-formula" ] [ Render.formulaHtml0 model.display ]
        , div
              [ class "dialog-control" ]
              [ button [ class "basic", onClick CancelClickedMsg ] [ text "Cancel" ]
              , div [ Html.Attributes.style [ ( "width", "8px" ) ] ] []
              , button
                    [ class "basic", disabled (List.any isNothing model.selectedTerms), onClick OkClickedMsg ]
                    [ text "OK" ]
              ]
        , div
              [ class "dialog-command" ]
              (List.indexedMap
                   (\i o ->
                        button
                            [ class ("circle-button-2" ++ selectedClass (i == model.selectedOption))
                            , onClick (SelectionClickedMsg i)
                            ]
                            [ Render.centerTermDiv (Term.Atom (Term.MetaVar (unsafeGet i model.option))) ]
                   )
                   model.option
              )
        , div
              [ id "scrolling", class "panel-body" ]
              [ listView (unsafeGet model.selectedOption model.selectedTerms) model.options ]
        ]


update : Msg -> Model -> Out
update msg model =
    case msg of
        EmptyMsg ->
            EmptyOut

        SelectionClickedMsg i1 ->
            if i1 == model.selectedOption then
                EmptyOut
            else
                let
                    newModel =
                        { model | selectedOption = i1, options = List.reverse opt }

                    opt =
                        List.foldl
                            (\( a3, b3 ) ts2 ->
                                 if v == a3 then
                                     b3
                                 else
                                     ts2
                            )
                            []
                            dict3

                    dict3 =
                        Transformations.populate model.focus model.scratch model.context rule1 model.transformation.isReversed dict2

                    dict2 =
                        List.map
                            (\( a2, b2 ) ->
                                 if v == a2 then
                                     ( a2, [] )
                                 else
                                     ( a2, b2 )
                            )
                            dict1

                    dict1 =
                        List.foldl
                            (\( a1, b1 ) d ->
                                 case b1 of
                                     Nothing ->
                                         d

                                     Just x1 ->
                                         fromJust (Transformations.dictionaryAddUnique a1 x1 d)
                            )
                            dict0
                            (List.map2 (\v0 v1 -> ( v0, v1 )) model.option model.selectedTerms)

                    dict0 =
                        Transformations.ruleDictionary rule1

                    rule1 =
                        Transformations.toRule 0 model.rule

                    v =
                        unsafeGet i1 model.option
                in
                ChangedOut (updateDisplay newModel)

        ListItemClickedMsg t1 ->
            let
                newModel =
                    if Just t1 == unsafeGet model.selectedOption model.selectedTerms then
                        { model | selectedTerms = Utilities.set model.selectedOption Nothing model.selectedTerms }
                    else
                        { model | selectedTerms = Utilities.set model.selectedOption (Just t1) model.selectedTerms }
            in
            ChangedOut (updateDisplay newModel)

        CancelClickedMsg ->
            CancelOut

        OkClickedMsg ->
            OkOut model.transformation model.display
