module Page.Introduction exposing (..)

import Graphics
import Html
import Html.Attributes
import Html.Events


view : msg -> a -> Html.Html msg
view msg version =
    Html.div
        [ Html.Attributes.class "content-body" ]
        [ Html.div
              [ Html.Attributes.class "nav-bar" ]
              [ Html.button
                    [ Html.Attributes.class "nav-bar-button", Html.Attributes.title "Back", Html.Events.onClick msg ]
                    [ Graphics.leftAngle ]
              ]
        , Html.div
              [ Html.Attributes.id "scrolling", Html.Attributes.class "introduction-box" ]
              [ Html.div
                    [ Html.Attributes.class "introduction-contents" ]
                    [ Html.p [] [ Html.text "This project can be viewed two different ways." ]
                    , Html.p
                          []
                          [ Html.text
                                "First, it provides a review of basic algebra. It covers all the fundamental rules, making it clear when you need to worry about things like division by zero. By the end, you should have no confusion about what you can and can't do."
                          ]
                    , Html.p
                          []
                          [ Html.text
                                "Second, it is designed to work as a puzzle game. You are asked to figure out all tactics. There are no recipes to follow."
                          ]
                    , Html.p
                          []
                          [ Html.text
                                "For mathematicians, it is mostly in the style of the simplest problems seen in a class on abstract algebra, except instead of saying the word field I just say algebra and stick with the reals. Every problem is a proof of equivalence between an initial formula and a goal formula. The rules presented follow the traditional field axioms. Some changes have been made to make all this feel as close to traditional middle-school algebra as possible."
                          ]
                    ]
              ]
        ]
