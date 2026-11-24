module Page.About exposing (..)

import Graphics
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view : msg -> a -> Html msg
view msg version =
    Html.div
        [ Html.Attributes.class "content-body" ]
        [ Html.div
              [ Html.Attributes.class "nav-bar" ]
              [ Html.button
                    [ Html.Attributes.class "nav-bar-button", Html.Attributes.title "Back", Html.Events.onClick msg ]
                    [ Graphics.leftAngle ]
              ]
        , Html.div [ Html.Attributes.class "grow" ] []
        , Html.div
              [ Html.Attributes.class "about-box" ]
              [ Html.div [ Html.Attributes.class "about-title" ] [ Html.text "Sympathy for the Machine" ]
              , Html.div [ Html.Attributes.class "about-subtitle" ] [ Html.text "A Tactical Approach to Basic Algebra" ]
              , Html.div [ Html.Attributes.class "about-created" ] [ Html.text "Made by Edwin Zacharias" ]
              , Html.div
                    [ Html.Attributes.class "about-email" ]
                    [ Html.a
                          [ Html.Attributes.href "mailto:ezacharias@sympathyforthemachine.com" ]
                          [ Html.text "ezacharias@sympathyforthemachine.com" ]
                    ]
              ]
        , Html.div [ Html.Attributes.class "grow-2" ] []
        , Html.div [ Html.Attributes.class "about-copyright" ] [ Html.text "Copyright © 2018 Edwin Zacharias." ]
        ]
