module Dialog.Number exposing (..)

import Nat exposing (Nat)
import Term exposing (Term)
import Html
import Html.Attributes
import Html.Events
import Render


type alias Model =
    { number : Nat
    }


type Msg
    = IgnoreMsg
    | DigitClickedMsg Int
    | OkClickedMsg
    | CancelClickedMsg


type Out
    = NoChangeOut
    | ChangedOut Model
    | ExitOut
    | AddOut Term


init : Model
init =
    { number = Nat.zero }


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.class "content-body" ]
        [ Html.div
              [ Html.Attributes.class "nav-bar" ]
              [ Html.div [] []
              , Html.div [ Html.Attributes.class "top-title" ] [ Html.text "Number" ]
              , Html.div [] []
              ]
        , Html.div [ Html.Attributes.class "main-formula" ] [ Render.formulaHtml0 (Term.Atom (Term.Num model.number)) ]
        , Html.div
              [ Html.Attributes.class "dialog-control" ]
              [ Html.button [ Html.Attributes.class "basic", Html.Events.onClick CancelClickedMsg ] [ Html.text "Cancel" ]
              , Html.div [ Html.Attributes.style [ ( "width", "8px" ) ] ] []
              , Html.button [ Html.Attributes.class "basic", Html.Events.onClick OkClickedMsg ] [ Html.text "OK" ]
              ]
        , Html.div
              [ Html.Attributes.class "number-pad" ]
              [ Html.div
                    [ Html.Attributes.class "number-pad-row" ]
                    [ Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 7) ]
                          [ Html.text "7" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 8) ]
                          [ Html.text "8" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 9) ]
                          [ Html.text "9" ]
                    ]
              , Html.div
                    [ Html.Attributes.class "number-pad-row" ]
                    [ Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 4) ]
                          [ Html.text "4" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 5) ]
                          [ Html.text "5" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 6) ]
                          [ Html.text "6" ]
                    ]
              , Html.div
                    [ Html.Attributes.class "number-pad-row" ]
                    [ Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 1) ]
                          [ Html.text "1" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 2) ]
                          [ Html.text "2" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 3) ]
                          [ Html.text "3" ]
                    ]
              , Html.div
                    [ Html.Attributes.class "number-pad-row" ]
                    [ Html.button [ Html.Attributes.class "circle-button-2 selected hidden" ] [ Html.text "0" ]
                    , Html.button
                          [ Html.Attributes.class "circle-button-2 selected", Html.Events.onClick (DigitClickedMsg 0) ]
                          [ Html.text "0" ]
                    , Html.button [ Html.Attributes.class "circle-button-2 selected hidden" ] [ Html.text "0" ]
                    ]
              ]
        ]


update : Msg -> Model -> Out
update msg model =
    case msg of
        IgnoreMsg ->
            NoChangeOut

        DigitClickedMsg i1 ->
            if i1 == 0 && Nat.eq model.number Nat.zero then
                NoChangeOut
            else
                ChangedOut { model | number = Nat.add (Nat.mul model.number (Nat.fromInt 10)) (Nat.fromInt i1) }

        CancelClickedMsg ->
            ExitOut

        OkClickedMsg ->
            AddOut (Term.Atom (Term.Num model.number))
