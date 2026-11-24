module Page.List exposing (..)

import Graphics
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utilities exposing (..)
import Problem exposing (..)
import Json.Decode


type alias Model =
    List Problem


type Msg
    = GoToProblemMsg Int
    | GoToIntroductionMsg
    | GoToAboutMsg
    | OnScrollMsg


viewTombstone : Bool -> Html Msg
viewTombstone isSolved =
    if isSolved then
        Graphics.whiteTombstone
    else
        Graphics.dashedTombstone


viewAbout : Html Msg
viewAbout =
    div [ class "clickable listing-about-item", onClick GoToAboutMsg ]
        [ text "About" ]


viewIntro : Html Msg
viewIntro =
    Html.div
        [ Html.Attributes.class "clickable listing-intro-item", Html.Events.onClick GoToIntroductionMsg ]
        [ Html.text "Introduction" ]


viewItem : Int -> Problem -> Html Msg
viewItem i p =
    li [ class "clickable listing-problem-item", onClick <| GoToProblemMsg i ]
        [ div [ class "listing-tombstone" ]
            [ viewTombstone p.solved
            ]
        , div [ class "listing-description" ]
            [ div [ class "listing-problem-text" ] [ text <| "Problem " ++ toString (i + 1) ]
            , div [ class "listing-title-text" ] [ text p.description ]
            ]
        ]


view : Model -> Html Msg
view ps =
    div [ class "content-body" ]
        [ div [ id "scrolling", onScroll OnScrollMsg ]
            [ ol [ class "top-list" ] (List.indexedMap viewItem ps)
            , viewEditions
            , viewAbout
            ]
        ]


{-| Link back to the page where you choose between the classic and the algebra
edition. The problem list is always shown at the root of an edition, so the
parent directory is the edition chooser.
-}
viewEditions : Html Msg
viewEditions =
    a [ class "listing-editions-item", href "../" ]
        [ text "All editions" ]
