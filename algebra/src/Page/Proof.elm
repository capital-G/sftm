module Page.Proof exposing (..)

import Graphics
import Html exposing (Html, div, span, text, button, ol, li)
import Html.Attributes exposing (id, class, title, disabled, style)
import Html.Events exposing (onClick)
import Render
import Rule exposing (Rule)
import Step exposing (Step)
import Term exposing (Term)
import Utilities exposing (unsafeGet)


type alias Model =
    { name : String
    , index : Int
    , start : Term
    , steps : List Step
    , rules : List Rule
    }


viewStep : a -> Model -> number -> { d | isReversed : Bool, rule : Int, term : Term } -> Html msg
viewStep formulaWidth model i step =
    li
        [ class "history-item" ]
        [ div
              [ class "history-text" ]
              [ span [ class "history-gutter" ] [ text (toString (i + 1)) ]
              , text
                    ((unsafeGet step.rule model.rules).name
                         ++ (if step.isReversed then
                                 " (reverse)"
                             else
                                 ""
                            )
                    )
              ]
        , div [ class "history-formula" ] [ Render.leftTermDiv step.term ]
        ]


viewStart : a -> Model -> Html msg
viewStart formulaWidth model =
    li
        [ class "history-item" ]
        [ div [ class "history-text" ] [ span [ class "history-gutter" ] [ text "0" ], text "Given" ]
        , div [ class "history-formula" ] [ Render.leftTermDiv model.start ]
        ]


view : msg -> Model -> Html msg
view msg model =
    let
        width =
            List.foldl max 0 (List.map .width (startShape :: shapes))

        startShape =
            Render.midTermShape model.start

        shapes =
            List.map (\p0 -> Render.midTermShape (.term p0)) model.steps
    in
    div
        [ class "content-body" ]
        [ div
              [ class "nav-bar" ]
              [ button [ class "nav-bar-button", title "Back", onClick msg ] [ Graphics.leftAngle ] ]
        , div [ class "proof-description" ] [ text ("Problem " ++ toString (model.index + 1) ++ ": " ++ model.name) ]
        , Render.centerTermDiv (Term.Binary Term.Equivalence model.start (Utilities.last model.steps).term)
        , div [ class "proof-type" ] [ text "Proof: By chain of equivalence." ]
        , div
              [ id "scrolling", class "bottom-list" ]
              [ ol [ class "proof-list" ] (viewStart width model :: List.indexedMap (viewStep width model) model.steps)
              , div [ class "proof-tombstone" ] [ Graphics.whiteTombstone ]
              ]
        ]
