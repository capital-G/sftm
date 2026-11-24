module Render exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Path exposing (Path)
import Rule exposing (Rule)
import Svg as S exposing (Svg, Attribute)
import Svg.Attributes as SA
import Term exposing (..)
import Transformation exposing (Transformation)
import Utilities exposing (castHtml, ariaHidden)
import MathML
import MathML.Attributes
import Nat


type alias Shape =
    { width : Float
    , above : Float
    , below : Float
    , svg : Transform -> List (Svg Never)
    , focus : Maybe Rect
    , assoc : Maybe Rect
    , mathML : List (Html Never)
    }


type alias Rect =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Glyph =
    { id : String
    , ascent : Float
    , descent : Float
    , width : Float
    , xStart : Float
    , xEnd : Float
    }


type alias Transform =
    { x : Float
    , y : Float
    }


type alias Context =
    { precedence : Int
    , focusPosition : Position
    , assocPosition : Position
    }


type Position
    = Path Path
    | Inside
    | Outside


raiseY : Float
raiseY =
    90


midline : Float
midline =
    250 + raiseY


shapeRect : Shape -> Rect
shapeRect shape =
    case shape.focus of
        Nothing ->
            { x = 0.0, y = 0 - shape.below, width = shape.width, height = shape.below + shape.above }

        Just x1 ->
            x1


blackboardRGlyph : Glyph
blackboardRGlyph =
    { id = "#MJAMS-52", ascent = 683, descent = 1, width = 722, xStart = 16, xEnd = 705 }


frakturCGlyph : Glyph
frakturCGlyph =
    { id = "#MJFRAK-43", ascent = 685, descent = 24, width = 613, xStart = 59, xEnd = 607 }


frakturBGlyph : Glyph
frakturBGlyph =
    { id = "#MJFRAK-42", ascent = 691, descent = 27, width = 884, xStart = 48, xEnd = 820 }


frakturAGlyph : Glyph
frakturAGlyph =
    { id = "#MJFRAK-41", ascent = 696, descent = 26, width = 718, xStart = 22, xEnd = 707 }


frakturLowerDGlyph : Glyph
frakturLowerDGlyph =
    { id = "#MJFRAK-64", ascent = 609, descent = 33, width = 499, xStart = 13, xEnd = 428 }


frakturLowerCGlyph : Glyph
frakturLowerCGlyph =
    { id = "#MJFRAK-63", ascent = 466, descent = 29, width = 389, xStart = 72, xEnd = 358 }


frakturLowerBGlyph : Glyph
frakturLowerBGlyph =
    { id = "#MJFRAK-62", ascent = 685, descent = 31, width = 513, xStart = 87, xEnd = 442 }


frakturLowerAGlyph : Glyph
frakturLowerAGlyph =
    { id = "#MJFRAK-61", ascent = 470, descent = 35, width = 500, xStart = 66, xEnd = 497 }


digit9Glyph : Glyph
digit9Glyph =
    { id = "#MJMAIN-39", ascent = 666, descent = 22, width = 500, xStart = 42, xEnd = 456 }


digit8Glyph : Glyph
digit8Glyph =
    { id = "#MJMAIN-38", ascent = 666, descent = 22, width = 500, xStart = 43, xEnd = 457 }


digit7Glyph : Glyph
digit7Glyph =
    { id = "#MJMAIN-37", ascent = 676, descent = 22, width = 500, xStart = 55, xEnd = 485 }


digit6Glyph : Glyph
digit6Glyph =
    { id = "#MJMAIN-36", ascent = 666, descent = 22, width = 500, xStart = 41, xEnd = 456 }


digit5Glyph : Glyph
digit5Glyph =
    { id = "#MJMAIN-35", ascent = 666, descent = 22, width = 500, xStart = 50, xEnd = 449 }


digit4Glyph : Glyph
digit4Glyph =
    { id = "#MJMAIN-34", ascent = 677, descent = 0, width = 500, xStart = 28, xEnd = 471 }


digit3Glyph : Glyph
digit3Glyph =
    { id = "#MJMAIN-33", ascent = 665, descent = 22, width = 500, xStart = 42, xEnd = 457 }


digit2Glyph : Glyph
digit2Glyph =
    { id = "#MJMAIN-32", ascent = 666, descent = 0, width = 500, xStart = 83, xEnd = 427 }


digit1Glyph : Glyph
digit1Glyph =
    { id = "#MJMAIN-31", ascent = 666, descent = 0, width = 500, xStart = 83, xEnd = 427 }


digit0Glyph : Glyph
digit0Glyph =
    { id = "#MJMAIN-30", ascent = 666, descent = 22, width = 500, xStart = 39, xEnd = 460 }


starGlyph : Glyph
starGlyph =
    { id = "#MJAMS-2605", ascent = 694, descent = 111, width = 944, xStart = 49, xEnd = 895 }


commaGlyph : Glyph
commaGlyph =
    { id = "#MJMAIN-2C", ascent = 121, descent = 195, width = 278, xStart = 78, xEnd = 210 }


translateGlyph : Float -> Float -> Glyph -> Glyph
translateGlyph x y g =
    { g | ascent = g.ascent + y, descent = g.descent - y, xStart = g.xStart + x, xEnd = g.xEnd + x }


latinUpperAGlyph : Glyph
latinUpperAGlyph =
    translateGlyph (-40) (0) { id = "#MJMATHI-41", ascent = 716, descent = 0, width = 743, xStart = 58, xEnd = 696 }


latinUpperBGlyph : Glyph
latinUpperBGlyph =
    translateGlyph (-40) (0) { id = "#MJMATHI-42", ascent = 683, descent = 0, width = 704, xStart = 57, xEnd = 732 }


latinUpperCGlyph : Glyph
latinUpperCGlyph =
    translateGlyph (-40) (0) { id = "#MJMATHI-43", ascent = 705, descent = 21, width = 716, xStart = 150, xEnd = 812 }


latinLowerAGlyph : Glyph
latinLowerAGlyph =
    translateGlyph
        (-40)
        0
        { id = "#MJMATHI-61", ascent = 441, descent = 10, width = 529, xStart = 33, xEnd = 506 }


latinLowerBGlyph : Glyph
latinLowerBGlyph =
    translateGlyph
        (-40)
        0
        { id = "#MJMATHI-62", ascent = 694, descent = 11, width = 429, xStart = 40, xEnd = 422 }


latinLowerCGlyph : Glyph
latinLowerCGlyph =
    translateGlyph
        (-40)
        0
        { id = "#MJMATHI-63", ascent = 442, descent = 12, width = 433, xStart = 34, xEnd = 430 }


latinLowerXGlyph : Glyph
latinLowerXGlyph =
    translateGlyph
        (-40)
        0
        { id = "#MJMATHI-78", ascent = 442, descent = 11, width = 572, xStart = 35, xEnd = 522 }


latinLowerYGlyph : Glyph
latinLowerYGlyph =
    translateGlyph
        (-40)
        0
        { id = "#MJMATHI-79", ascent = 443, descent = 205, width = 490, xStart = 21, xEnd = 497 }


metaColor : String
metaColor =
    "Orchid"


nonFocusColor : String
nonFocusColor =
    "SlateGrey"


assocColor : String
assocColor =
    "Yellow"


focusColor : String
focusColor =
    "White"


contextIsTrueAssoc : Context -> Bool
contextIsTrueAssoc c =
    case c.assocPosition of
        Path [] ->
            True

        Path _ ->
            False

        Inside ->
            False

        Outside ->
            False


contextIsTrueFocus : Context -> Bool
contextIsTrueFocus c =
    case c.focusPosition of
        Path [] ->
            True

        Path _ ->
            False

        Inside ->
            False

        Outside ->
            False


contextIsFocus : Context -> Bool
contextIsFocus c =
    case c.focusPosition of
        Path [] ->
            True

        Path _ ->
            False

        Inside ->
            True

        Outside ->
            False


ifFocus : Context -> List (Html msg) -> List (Html msg)
ifFocus ctx m =
    if contextIsFocus ctx then
        m
    else
        []


contextIsMathML : Context -> Bool
contextIsMathML c =
    case c.focusPosition of
        Path [] ->
            True

        Path _ ->
            True

        Inside ->
            True

        Outside ->
            False


scaleRect : number -> Maybe { a | width : number, x : number, y : number, height : number } -> Maybe { a | height : number, width : number, x : number, y : number }
scaleRect s =
    Maybe.map (\r -> { r | x = r.x * s, y = r.y * s, width = r.width * s, height = r.height * s })


translateRectXY : number -> number1 -> Maybe { a | x : number, y : number1 } -> Maybe { a | x : number, y : number1 }
translateRectXY x y =
    Maybe.map (\r -> { r | x = r.x + x, y = r.y + y })


translateRectY : Float -> Maybe Rect -> Maybe Rect
translateRectY y =
    Maybe.map (\r -> { r | y = r.y + y })


reallyRaiseShape : Float -> Shape -> Shape
reallyRaiseShape y s =
    { s
        | above = s.above + y
        , below = s.below - y
        , svg = \t -> s.svg { t | y = t.y + y }
        , focus = translateRectY y s.focus
        , assoc = translateRectY y s.assoc
    }


raiseShape : Shape -> Shape
raiseShape =
    reallyRaiseShape raiseY


translateRectX : Float -> Maybe Rect -> Maybe Rect
translateRectX x =
    Maybe.map (\r -> { r | x = r.x + x })


maybes : Maybe a -> Maybe a -> Maybe a
maybes m1 m2 =
    case m1 of
        Just x ->
            Just x

        Nothing ->
            m2


apposition : Shape -> Shape -> Shape
apposition s1 s2 =
    { width = s1.width + 100 + s2.width
    , above = max s1.above s2.above
    , below = max s1.below s2.below
    , svg = \tr -> s1.svg tr ++ s2.svg { x = tr.x + s1.width + 100, y = tr.y }
    , focus = maybes s1.focus (translateRectX (s1.width + 100) s2.focus)
    , assoc = maybes s1.assoc (translateRectX (s1.width + 100) s2.assoc)
    , mathML = s1.mathML ++ s2.mathML
    }


contextParenColor : Context -> String
contextParenColor c =
    case ( c.focusPosition, c.assocPosition ) of
        ( Inside, _ ) ->
            focusColor

        ( _, Inside ) ->
            assocColor

        ( _, _ ) ->
            nonFocusColor


contextColor : Context -> String
contextColor c =
    case ( c.focusPosition, c.assocPosition ) of
        ( Path [], _ ) ->
            focusColor

        ( Inside, _ ) ->
            focusColor

        ( _, Path [] ) ->
            assocColor

        ( _, Inside ) ->
            assocColor

        ( _, _ ) ->
            nonFocusColor


rToStr : Float -> Float -> String
rToStr m n =
    let
        i =
            round ((abs n) * (1000 / m))

        r =
            i % 1000

        d =
            if n < 0 then
                "-"
            else
                ""

        e =
            if r == 0 then
                ""
            else if r < 10 then
                ".00" ++ toString r
            else if r < 100 then
                ".0" ++ toString r
            else
                "." ++ toString r
    in
        d ++ toString (i // 1000) ++ e


toStr2 : Float -> String
toStr2 =
    rToStr 950


toStr : Float -> String
toStr =
    rToStr 850


toS : Float -> String
toS =
    round >> toString


shapeHtml : Shape -> Html msg
shapeHtml shape =
    castHtml <|
        S.svg
            [ SA.style <|
                ("transform: scale(1,-1);")
                    ++ ("width: " ++ toStr shape.width ++ "em" ++ ";")
                    ++ ("height: " ++ toStr (shape.below + shape.above) ++ "em" ++ ";")
                    ++ ("flex-shrink: 1;")
            , ariaHidden True
            , SA.fill "none"
            , SA.viewBox <| toS 0.0 ++ " " ++ toS (0.0 - shape.below) ++ " " ++ toS shape.width ++ " " ++ toS (shape.below + shape.above)
            ]
            (shape.svg { x = 0.0, y = 0.0 })


formulaShapeView : Shape -> Html a
formulaShapeView shape =
    let
        svg =
            castHtml
                (S.svg
                     [ SA.class "center-formula"
                     , SA.fill "none"
                     , SA.viewBox
                           (toS (rect.x - buffer) ++ " " ++ toS rect.y ++ " " ++ toS (buffer + rect.width + buffer) ++ " " ++ toS rect.height)
                     ]
                     (shape.svg { x = 0.0, y = 0.0 })
                )

        rect =
            shapeRect shape

        buffer =
            500
    in
    H.div
        [ HA.class "formula-svg-box" ]
        [ svg
        , H.span
              [ HA.style [ ( "display", "block" ), ( "overflow", "hidden" ), ( "height", "1px" ), ( "width", "1px" ) ]
              ]
              [ castHtml (MathML.math [] shape.mathML) ]
        ]


fractionShape : Context -> Shape -> Shape -> Shape
fractionShape ctx s1 s2 =
    let
        s2y =
            250 - (100 + s2.above * s2t)

        s2x =
            (width - s2.width * s2t) / 2

        lineSvg tr =
            [ S.path
                  [ SA.stroke (contextColor ctx)
                  , SA.strokeWidth (toString 40)
                  , SA.strokeLinecap "round"
                  , SA.d
                        (("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 250)) ++ "L" ++ toS (tr.x + width - 100) ++ " " ++ toS (tr.y + 250))
                  ]
                  []
            ]

        s1x =
            (width - s1.width * s1t) / 2

        width =
            100 + max (s1.width * s1t) (s2.width * s2t) + 100

        s2t =
            min 1 (maxHeight / (s2.above + s2.below))

        s1y =
            250 + (100 + s1.below * s1t)

        s1t =
            min 1 (maxHeight / (s1.above + s1.below))

        maxHeight =
            650
    in
    raiseShape
        { width = width
        , above = 250 + 100 + s1.below * s1t + s1.above * s1t
        , below = s2.below * s2t + s2.above * s2t + 100 - 250
        , svg =
              \tr ->
                  [ S.g
                        [ SA.transform
                              ("translate(" ++ toS (tr.x + s1x) ++ "," ++ toS (tr.y + s1y) ++ ") scale(" ++ toString s1t ++ ")")
                        ]
                        (s1.svg { x = 0, y = 0 })
                  ]
                      ++ lineSvg tr
                             ++ [ S.g
                                      [ SA.transform
                                            ("translate(" ++ toS (tr.x + s2x) ++ "," ++ toS (tr.y + s2y) ++ ") scale(" ++ toString s2t ++ ")")
                                      ]
                                      (s2.svg { x = 0, y = 0 })
                                ]
        , focus = maybes (translateRectXY s1x s1y (scaleRect s1t s1.focus)) (translateRectXY s2x s2y (scaleRect s2t s2.focus))
        , assoc = maybes (translateRectXY s1x s1y (scaleRect s1t s1.assoc)) (translateRectXY s2x s2y (scaleRect s2t s2.assoc))
        , mathML = []
        }


squareShape : String -> Shape
squareShape color =
    raiseShape
        { width = 550.0
        , above = 500.0
        , below = 0.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke color
                        , SA.fill color
                        , SA.strokeLinecap "round"
                        , SA.strokeWidth (toString 40)
                        , SA.d
                              (("M" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 50))
                                   ++ ("L" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 450))
                                          ++ ("L" ++ toS (tr.x + 475) ++ " " ++ toS (tr.y + 450))
                                                 ++ ("L" ++ toS (tr.x + 475) ++ " " ++ toS (tr.y + 50)) ++ "Z"
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = []
        }


shapeHeight : Shape -> Float
shapeHeight s =
    s.below + s.above


max3 : Float -> Float -> Float -> Float
max3 f1 f2 f3 =
    max f1 (max f2 f3)


longLeftRightArrowShape : Shape
longLeftRightArrowShape =
    raiseShape
        { width = 1859.0
        , above = 511.0
        , below = 11.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke "White"
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.strokeWidth (toString 40)
                    , SA.d
                        (("M" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 1783) ++ " " ++ toS (tr.y + 250))
                            ++ ("M" ++ toS (tr.x + 285) ++ " " ++ toS (tr.y + 40))
                            ++ ("L" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 285) ++ " " ++ toS (tr.y + 460))
                            ++ ("M" ++ toS (tr.x + 1573) ++ " " ++ toS (tr.y + 40))
                            ++ ("L" ++ toS (tr.x + 1783) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 1573) ++ " " ++ toS (tr.y + 460))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mo [] [ H.text "⟷" ] ]
        }



-- Utilities


leftRightArrowShape : Context -> Shape
leftRightArrowShape ctx =
    raiseShape
        { width = 987.0
        , above = 511.0
        , below = 11.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.strokeWidth (toString 40)
                    , SA.d
                        (("M" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 911) ++ " " ++ toS (tr.y + 250))
                            ++ ("M" ++ toS (tr.x + 285) ++ " " ++ toS (tr.y + 40))
                            ++ ("L" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 285) ++ " " ++ toS (tr.y + 460))
                            ++ ("M" ++ toS (tr.x + 701) ++ " " ++ toS (tr.y + 40))
                            ++ ("L" ++ toS (tr.x + 911) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 701) ++ " " ++ toS (tr.y + 460))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "↔" ] ]
        }


turnstileShape : Shape
turnstileShape =
    raiseShape
        { width = 600.0
        , above = 550.0
        , below = 50.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke "White"
                    , SA.strokeLinecap "round"
                    , SA.strokeWidth (toString 40)
                    , SA.d
                        ("M"
                            ++ toS (tr.x + 70)
                            ++ " "
                            ++ toS (tr.y - 30)
                            ++ "L"
                            ++ toS (tr.x + 70)
                            ++ " "
                            ++ toS (tr.y + 530)
                            ++ "M"
                            ++ toS (tr.x + 70)
                            ++ " "
                            ++ toS (tr.y + 250)
                            ++ "L"
                            ++ toS (tr.x + 530)
                            ++ " "
                            ++ toS (tr.y + 250)
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mo [] [ H.text "⊢" ] ]
        }


slashScale : Float
slashScale =
    0.67


slashShape : Context -> Shape -> Shape -> Shape
slashShape ctx s1 s2 =
    raiseShape
        { width = s1.width * slashScale + 200 + s2.width * slashScale
        , above = 750.0
        , below = 50.0
        , svg =
              \tr ->
                  [ S.g
                        [ SA.transform
                              ("translate("
                                   ++ toS (tr.x + 50) ++ "," ++ toS (tr.y + 750 - 750 * slashScale) ++ ") scale(" ++ toString slashScale ++ ")"
                              )
                        ]
                        (s1.svg { x = 0, y = 0 })
                  , S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeLinecap "round"
                        , SA.strokeWidth (toString 40)
                        , SA.d
                              (("M" ++ toS (tr.x + 50 + s1.width * slashScale + 0) ++ " " ++ toS (tr.y - 30))
                                   ++ "L" ++ toS (tr.x + 50 + s1.width * slashScale + 250) ++ " " ++ toS (tr.y + 730)
                              )
                        ]
                        []
                  , S.g
                        [ SA.transform
                              ("translate("
                                   ++ toS (tr.x + 50 + s1.width * slashScale + 250) ++ "," ++ toS tr.y ++ ") scale(" ++ toString slashScale ++ ")"
                              )
                        ]
                        (s2.svg { x = 0, y = 0 })
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "∧" ] ]
        }


digitGlyphs : List Glyph
digitGlyphs =
    [ digit0Glyph
    , digit1Glyph
    , digit2Glyph
    , digit3Glyph
    , digit4Glyph
    , digit5Glyph
    , digit6Glyph
    , digit7Glyph
    , digit8Glyph
    , digit9Glyph
    ]


plusShape : Context -> Shape
plusShape ctx =
    raiseShape
        { width = 600.0
        , above = 550.0
        , below = 50.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeLinecap "round"
                        , SA.strokeWidth (toString 40)
                        , SA.d
                              (("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 250))
                                   ++ ("L" ++ toS (tr.x + 550) ++ " " ++ toS (tr.y + 250))
                                          ++ ("M" ++ toS (tr.x + 310) ++ " " ++ toS (tr.y - 30)) ++ "L" ++ toS (tr.x + 310) ++ " " ++ toS (tr.y + 530)
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "∧" ] ]
        }


notEqualShape : Context -> Shape
notEqualShape ctx =
    raiseShape
        { width = 600.0
        , above = 550.0
        , below = 50.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeLinecap "round"
                        , SA.strokeWidth (toString 40)
                        , SA.d
                              (("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 150))
                                   ++ ("L" ++ toS (tr.x + 550) ++ " " ++ toS (tr.y + 150))
                                          ++ ("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 350))
                                                 ++ ("L" ++ toS (tr.x + 550) ++ " " ++ toS (tr.y + 350))
                                                        ++ ("M" ++ toS (tr.x + 100) ++ " " ++ toS (tr.y + 0)) ++ "L" ++ toS (tr.x + 500) ++ " " ++ toS (tr.y + 500)
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "=" ] ]
        }


equalShape : Context -> Shape
equalShape ctx =
    raiseShape
        { width = 600.0
        , above = 550.0
        , below = 50.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeLinecap "round"
                        , SA.strokeWidth (toString 40)
                        , SA.d
                              (("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 150))
                                   ++ ("L" ++ toS (tr.x + 550) ++ " " ++ toS (tr.y + 150))
                                          ++ ("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 350)) ++ "L" ++ toS (tr.x + 550) ++ " " ++ toS (tr.y + 330)
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "=" ] ]
        }


conjunctionShape : Context -> Shape
conjunctionShape ctx =
    raiseShape
        { width = 660.0
        , above = 550.0
        , below = 50.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.strokeWidth (toString 40)
                    , SA.d
                        (("M" ++ toS (tr.x + 20) ++ " " ++ toS (tr.y - 30))
                            ++ ("L" ++ toS (tr.x + 330) ++ " " ++ toS (tr.y + 530))
                            ++ ("L" ++ toS (tr.x + 640) ++ " " ++ toS (tr.y - 30))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "∧" ] ]
        }


disjunctionShape : Context -> Shape
disjunctionShape ctx =
    raiseShape
        { width = 660.0
        , above = 550.0
        , below = 50.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.strokeWidth (toString 40)
                    , SA.d
                        (("M" ++ toS (tr.x + 20) ++ " " ++ toS (tr.y + 530))
                            ++ ("L" ++ toS (tr.x + 330) ++ " " ++ toS (tr.y - 30))
                            ++ ("L" ++ toS (tr.x + 640) ++ " " ++ toS (tr.y + 530))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "∨" ] ]
        }


implicationArrowShape : Context -> Shape
implicationArrowShape ctx =
    raiseShape
        { width = 959.0
        , above = 511.0
        , below = 11.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeWidth (toString 40)
                        , SA.strokeLinecap "round"
                        , SA.strokeLinejoin "round"
                        , SA.d
                              (("M" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                                   ++ ("L" ++ toS (tr.x + 883) ++ " " ++ toS (tr.y + 250))
                                          ++ ("M" ++ toS (tr.x + 673) ++ " " ++ toS (tr.y + 40))
                                                 ++ ("L" ++ toS (tr.x + 883) ++ " " ++ toS (tr.y + 250)) ++ "L" ++ toS (tr.x + 673) ++ " " ++ toS (tr.y + 460)
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "→" ] ]
        }


rightArrowShape : Context -> Shape
rightArrowShape ctx =
    raiseShape
        { width = 1159.0
        , above = 511.0
        , below = 11.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeWidth (toString 40)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.d
                        (("M" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 1083) ++ " " ++ toS (tr.y + 250))
                            ++ ("M" ++ toS (tr.x + 873) ++ " " ++ toS (tr.y + 40))
                            ++ ("L" ++ toS (tr.x + 1083) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 873) ++ " " ++ toS (tr.y + 460))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = ifFocus ctx [ MathML.mo [] [ H.text "→" ] ]
        }


upTackShape : Context -> Shape
upTackShape ctx =
    raiseShape
        { width = 790.0
        , above = 600.0
        , below = 90.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeLinecap "round"
                    , SA.strokeWidth (toString 40)
                    , SA.d
                        (("M" ++ toS (tr.x + 60) ++ " " ++ toS (tr.y - 70))
                            ++ ("L" ++ toS (tr.x + 720) ++ " " ++ toS (tr.y - 70))
                            ++ ("M" ++ toS (tr.x + 390) ++ " " ++ toS (tr.y - 60))
                            ++ ("L" ++ toS (tr.x + 390) ++ " " ++ toS (tr.y + 580))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mi [] [ H.text "⊥" ] ]
        }


downTackShape : Context -> Shape
downTackShape ctx =
    raiseShape
        { width = 790.0
        , above = 590.0
        , below = 100.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeWidth (toS 40)
                    , SA.strokeLinecap "round"
                    , SA.d
                        (("M" ++ toS (tr.x + 60) ++ " " ++ toS (tr.y + 570))
                            ++ ("L" ++ toS (tr.x + 720) ++ " " ++ toS (tr.y + 570))
                            ++ ("M" ++ toS (tr.x + 390) ++ " " ++ toS (tr.y - 80))
                            ++ ("L" ++ toS (tr.x + 390) ++ " " ++ toS (tr.y + 560))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mi [] [ H.text "⊤" ] ]
        }


reallyGlyphShape : Float -> Float -> String -> Html Never -> Glyph -> Shape
reallyGlyphShape xOffset yOffset color m glyph =
    { width = glyph.width
    , above = glyph.ascent
    , below = glyph.descent
    , svg = \tr -> [ S.use [ (SA.xlinkHref glyph.id), SA.x (toS (tr.x + xOffset)), SA.y (toS (tr.y + yOffset)), SA.fill color ] [] ]
    , focus = Nothing
    , assoc = Nothing
    , mathML = [ m ]
    }


glyphShape : String -> Html Never -> Glyph -> Shape
glyphShape =
    reallyGlyphShape 0 0


intShape : Context -> Int -> Shape
intShape ctx n =
    let
        n1 =
            n % 10
    in
    glyphShape (contextColor ctx) (MathML.mi [] []) (Utilities.unsafeGet n1 digitGlyphs)


notShape : Context -> Shape
notShape ctx =
    raiseShape
        { width = 775
        , above = 370.0
        , below = (-130.0)
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextColor ctx)
                    , SA.strokeWidth (toString 40)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.d
                        (("M" ++ toS (tr.x + 75) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 700) ++ " " ++ toS (tr.y + 250))
                            ++ ("L" ++ toS (tr.x + 700) ++ " " ++ toS (tr.y + 50))
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mi [] [ H.text "¬" ] ]
        }


notInShape : Context -> Shape
notInShape ctx =
    raiseShape
        { width = 375
        , above = 370.0
        , below = -130.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeWidth (toString 40)
                        , SA.strokeLinecap "round"
                        , SA.d
                              (("M" ++ toS (tr.x + 85) ++ " " ++ toS (tr.y + 250))
                                   ++ ("L" ++ toS (tr.x + 275) ++ " " ++ toS (tr.y + 250))
                                          ++ ("M" ++ toS (tr.x + 275) ++ " " ++ toS (tr.y + 50))
                                                 ++ ("A" ++ toS 200 ++ " " ++ toS 200 ++ " 0 0 0 " ++ toS (tr.x + 275) ++ " " ++ toS (tr.y + 450))
                                                        ++ ("M" ++ toS (tr.x + 30) ++ " " ++ toS (tr.y + 0)) ++ "L" ++ toS (tr.x + 430) ++ " " ++ toS (tr.y + 500)
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mi [] [ H.text "in" ] ]
        }


inShape : Context -> Shape
inShape ctx =
    raiseShape
        { width = 375
        , above = 370.0
        , below = -130.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeWidth (toString 40)
                        , SA.strokeLinecap "round"
                        , SA.d
                              (("M" ++ toS (tr.x + 85) ++ " " ++ toS (tr.y + 250))
                                   ++ ("L" ++ toS (tr.x + 275) ++ " " ++ toS (tr.y + 250))
                                          ++ ("M" ++ toS (tr.x + 275) ++ " " ++ toS (tr.y + 50))
                                                 ++ "A" ++ toS 200 ++ " " ++ toS 200 ++ " 0 0 0 " ++ toS (tr.x + 275) ++ " " ++ toS (tr.y + 450)
                              )
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mi [] [ H.text "in" ] ]
        }


minusShape : Context -> Shape
minusShape ctx =
    raiseShape
        { width = 600
        , above = 370.0
        , below = -130.0
        , svg =
              \tr ->
                  [ S.path
                        [ SA.stroke (contextColor ctx)
                        , SA.strokeWidth (toString 40)
                        , SA.strokeLinecap "round"
                        , SA.strokeLinejoin "round"
                        , SA.d
                              (("M" ++ toS (tr.x + 50) ++ " " ++ toS (tr.y + 250)) ++ "L" ++ toS (tr.x + 550) ++ " " ++ toS (tr.y + 250))
                        ]
                        []
                  ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = [ MathML.mi [] [ H.text "-" ] ]
        }


reallyBinaryShape : Shape -> Shape -> Shape -> Shape
reallyBinaryShape s1 gly s2 =
    { width = s1.width + 400 + gly.width + 400 + s2.width
    , above = max3 s1.above gly.above s2.above
    , below = max3 s1.below gly.below s2.below
    , svg =
        \tr ->
            (s1.svg tr
                ++ gly.svg { tr | x = tr.x + s1.width + 400 }
                ++ s2.svg { tr | x = tr.x + s1.width + 400 + gly.width + 400 }
            )
    , focus = maybes s1.focus (translateRectX (s1.width + 400 + gly.width + 400) s2.focus)
    , assoc = maybes s1.assoc (translateRectX (s1.width + 400 + gly.width + 400) s2.assoc)
    , mathML = s1.mathML ++ gly.mathML ++ s2.mathML
    }


reallyUnaryShape : Shape -> Shape -> Shape
reallyUnaryShape gly s =
    { width = gly.width + 100 + s.width
    , above = max gly.above s.above
    , below = max gly.below s.below
    , svg = \tr -> gly.svg { tr | x = tr.x } ++ s.svg { tr | x = tr.x + gly.width + 100 }
    , focus = translateRectX (gly.width + 100) s.focus
    , assoc = translateRectX (gly.width + 100) s.assoc
    , mathML = gly.mathML ++ s.mathML
    }


rightParenShape : Context -> Shape
rightParenShape ctx =
    raiseShape
        { width = 400
        , above = 250.0 + 750.0
        , below = (-250) + 750.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextParenColor ctx)
                    , SA.strokeWidth (toString 40)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.d
                        (("M" ++ toS (tr.x + 70) ++ " " ++ toS (tr.y + 270 - 750.0))
                            ++ ("A"
                                    ++ toS (1500 - 250)
                                    ++ " "
                                    ++ toS (1500 - 250)
                                    ++ " "
                                    ++ toS (0)
                                    ++ " "
                                    ++ toS (0)
                                    ++ " "
                                    ++ toS (1)
                                    ++ " "
                                    ++ toS (tr.x + 70)
                                    ++ " "
                                    ++ toS (tr.y + 230 + 750)
                               )
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = []
        }


leftParenShape : Context -> Shape
leftParenShape ctx =
    raiseShape
        { width = 400
        , above = 250.0 + 750.0
        , below = (-250) + 750.0
        , svg =
            \tr ->
                [ S.path
                    [ SA.stroke (contextParenColor ctx)
                    , SA.strokeWidth (toString 40)
                    , SA.strokeLinecap "round"
                    , SA.strokeLinejoin "round"
                    , SA.d
                        (("M" ++ toS (tr.x + 330) ++ " " ++ toS (tr.y + 270 - 750))
                            ++ ("A"
                                    ++ toS (1500 - 250)
                                    ++ " "
                                    ++ toS (1500 - 250)
                                    ++ " "
                                    ++ toS (0)
                                    ++ " "
                                    ++ toS (0)
                                    ++ " "
                                    ++ toS (0)
                                    ++ " "
                                    ++ toS (tr.x + 330)
                                    ++ " "
                                    ++ toS (tr.y + 230 + 750)
                               )
                        )
                    ]
                    []
                ]
        , focus = Nothing
        , assoc = Nothing
        , mathML = []
        }


superscriptShape : Shape -> Shape -> Shape
superscriptShape s1 s2 =
    let
        scale =
            0.5

        lower =
            450
    in
    { width = s2.width + s1.width * scale
    , above = max s2.above (lower + s1.above)
    , below = max s2.below (s1.below - lower)
    , svg =
          \tr ->
              s2.svg tr
                  ++ [ S.g
                           [ SA.transform
                                 ("translate(" ++ toS (tr.x + s2.width) ++ "," ++ toS (tr.y + lower) ++ ") scale(" ++ toString scale ++ ")")
                           ]
                           (s1.svg { x = 0, y = 0 })
                     ]
    , focus = s2.focus
    , assoc = s2.assoc
    , mathML = []
    }


subscriptShape : Shape -> Shape -> Shape
subscriptShape s1 s2 =
    let
        scale =
            0.5

        lower =
            150
    in
    { width = s2.width + s1.width * scale
    , above = max s2.above (s1.above * scale - 150)
    , below = max s2.below (s1.below * scale + 150)
    , svg =
          \tr ->
              s2.svg tr
                  ++ [ S.g
                           [ SA.transform
                                 ("translate(" ++ toS (tr.x + s2.width) ++ "," ++ toS (tr.y - 150) ++ ") scale(" ++ toString scale ++ ")")
                           ]
                           (s1.svg { x = 0, y = 0 })
                     ]
    , focus = s2.focus
    , assoc = s2.assoc
    , mathML = []
    }


commaSep : List Shape -> Shape
commaSep shapes =
    case shapes of
        [] ->
            Debug.crash "impossible"

        [ s ] ->
            s

        s :: ss ->
            let
                s_ =
                    commaSep ss

                svg tr =
                    (s.svg tr
                        ++ [ S.use [ (SA.xlinkHref commaGlyph.id), SA.x (toS (tr.x + s.width)), SA.y (toS tr.y), SA.fill "White" ] [] ]
                        ++ s_.svg { tr | x = tr.x + s.width + commaGlyph.width + 400, y = tr.y }
                    )
            in
                { width = s.width + commaGlyph.width + 400 + s_.width
                , above = max3 s.above commaGlyph.ascent s_.above
                , below = max3 s.below commaGlyph.descent s_.below
                , svg = svg
                , focus = Nothing
                , assoc = Nothing
                , mathML = List.concat (List.intersperse [ MathML.mo [] [ H.text "," ] ] (List.map (.mathML) shapes))
                }


emptyShape : Shape
emptyShape =
    { width = 0.0
    , above = 0.0
    , below = 0.0
    , svg = \tr -> []
    , focus = Nothing
    , assoc = Nothing
    , mathML = []
    }


space : Shape
space =
    { width = 600.0
    , above = 0.0
    , below = 0.0
    , svg = \tr -> []
    , focus = Nothing
    , assoc = Nothing
    , mathML = []
    }


horizontal : Shape -> Shape -> Shape
horizontal s1 s2 =
    { width = s1.width + s2.width
    , above = max s1.above s2.above
    , below = max s1.below s2.below
    , svg = \tr -> (s1.svg tr ++ s2.svg { x = tr.x + s1.width, y = tr.y })
    , focus = maybes s1.focus (translateRectX s1.width s2.focus)
    , assoc = maybes s1.assoc (translateRectX s1.width s2.assoc)
    , mathML = s1.mathML ++ s2.mathML
    }


expansion : Float
expansion =
    50


shapeSpan : Shape -> Html a
shapeSpan shape =
    castHtml <|
        H.span [ HA.style [ ( "display", "inline-block" ), ( "position", "relative" ), ( "width", toStr2 shape.width ++ "em" ) ] ]
            [ S.svg
                [ SA.style <|
                    "transform: scale(1,-1);"
                        ++ ("width: " ++ toStr2 (shape.width + expansion * 2) ++ "em" ++ ";")
                        ++ ("height: " ++ toStr2 (shape.below + shape.above + expansion * 2) ++ "em" ++ ";")
                        ++ "position: absolute;"
                        ++ ("bottom: " ++ toStr2 (0.0 - (shape.below + expansion)) ++ "em;")
                        ++ ("left: " ++ toStr2 (0.0 - expansion) ++ "em;")
                , ariaHidden True
                , SA.fill "none"
                , SA.viewBox <|
                    (toS 0.0 ++ " ")
                        ++ (toS (0.0 - (shape.below + expansion)) ++ " ")
                        ++ (toS (shape.width + expansion * 2) ++ " ")
                        ++ toS (shape.below + shape.above + expansion * 2)
                ]
                (shape.svg { x = expansion, y = 0.0 })
            , H.span [ HA.style [ ( "display", "block" ), ( "overflow", "hidden" ), ( "height", "1px" ), ( "width", "1px" ) ] ] [ MathML.math [] (shape.mathML) ]
            ]


frakturAHtml : Html msg
frakturAHtml =
    frakturAGlyph |> glyphShape metaColor (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "A" ]) |> shapeSpan


frakturLowerAHtml : Html a
frakturLowerAHtml =
    shapeSpan
        (glyphShape
             metaColor
             (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "a" ])
             frakturLowerAGlyph
        )


latinUpperAHtml : Html msg
latinUpperAHtml =
    latinUpperAGlyph |> reallyGlyphShape (-40) (0) focusColor (MathML.mi [] [ H.text "A" ]) |> shapeSpan


latinUpperBHtml : Html msg
latinUpperBHtml =
    latinUpperBGlyph |> reallyGlyphShape (-70) (0) focusColor (MathML.mi [] [ H.text "B" ]) |> shapeSpan


latinUpperCHtml : Html msg
latinUpperCHtml =
    latinUpperCGlyph |> reallyGlyphShape (-40) (0) focusColor (MathML.mi [] [ H.text "C" ]) |> shapeSpan


longLeftRightArrowHtml : Html msg
longLeftRightArrowHtml =
    longLeftRightArrowShape |> shapeSpan


turnstileHtml : Html msg
turnstileHtml =
    turnstileShape |> shapeSpan


expandShape : Shape -> Shape
expandShape shape =
    { shape
        | width = shape.width + expansion * 2
        , below = shape.below + expansion
        , above = shape.above + expansion
        , svg =
            \tr ->
                shape.svg
                    { tr
                        | x = tr.x + expansion
                    }
    }


centerDiv : Shape -> Html msg
centerDiv shape =
    H.div [ HA.class "center-svg-box" ]
        [ shapeHtml shape
        , castHtml <| H.span [ HA.style [ ( "display", "block" ), ( "overflow", "hidden" ), ( "height", "1px" ), ( "width", "1px" ) ] ] [ MathML.math [] (shape.mathML) ]
        ]


starDiv : Html msg
starDiv =
    glyphShape metaColor (MathML.mo [] [ H.text "⋆" ]) starGlyph |> centerDiv


leftDiv : Shape -> Html msg
leftDiv shape =
    H.div [ HA.class "left-svg-box" ]
        [ shapeHtml shape
        , castHtml <| H.span [ HA.style [ ( "display", "block" ), ( "overflow", "hidden" ), ( "height", "1px" ), ( "width", "1px" ) ] ] [ MathML.math [] (shape.mathML) ]
        ]


leftRuleDivDots : Html msg
leftRuleDivDots =
    let
        shape =
            List.foldr horizontal emptyShape [ turnstileShape, space, star, space, longLeftRightArrowShape, space, star ]

        star =
            squareShape metaColor
    in
    leftDiv (expandShape { shape | above = midline + 750, below = 750 - midline })


lineHeight : number
lineHeight =
    1000


defaultContext : Context
defaultContext =
    { precedence = 0, focusPosition = Inside, assocPosition = Outside }


topHtml : Html msg
topHtml =
    downTackShape defaultContext |> shapeSpan


botHtml : Html msg
botHtml =
    upTackShape defaultContext |> shapeSpan


equivalenceHtml : Html msg
equivalenceHtml =
    leftRightArrowShape defaultContext |> shapeSpan


conjunctionHtml : Html msg
conjunctionHtml =
    conjunctionShape defaultContext |> shapeSpan


disjunctionHtml : Html msg
disjunctionHtml =
    disjunctionShape defaultContext |> shapeSpan


notHtml : Html a
notHtml =
    shapeSpan (notShape defaultContext)


implicationHtml : Html a
implicationHtml =
    shapeSpan (implicationArrowShape defaultContext)


ifInnerFocus : Context -> List (Html msg) -> List (Html msg)
ifInnerFocus ctx m =
    if ctx.focusPosition == Inside then
        m
    else
        []


parens : Context -> Shape -> Shape
parens ctx shape =
    { width = 400 + shape.width + 400
    , above = 250 + 750
    , below = (-250) + 750
    , svg =
        \tr ->
            ((leftParenShape ctx).svg tr
                ++ shape.svg { tr | x = tr.x + 400 }
                ++ (rightParenShape ctx).svg { tr | x = (tr.x + 400 + shape.width) }
            )
    , focus = translateRectX 400 shape.focus
    , assoc = translateRectX 400 shape.assoc
    , mathML = ifInnerFocus ctx [ MathML.mo [] [ H.text "(" ] ] ++ shape.mathML ++ ifInnerFocus ctx [ MathML.mo [] [ H.text ")" ] ]
    }


contextInLeft : Int -> Context -> Context
contextInLeft i c =
    let
        focusPosition =
            case c.focusPosition of
                Path [] ->
                    Inside

                Path (Path.GoLeft :: path2) ->
                    Path path2

                Path (Path.GoRight :: path2) ->
                    Outside

                Inside ->
                    Inside

                Outside ->
                    Outside

        assocPosition =
            case c.assocPosition of
                Path [] ->
                    Inside

                Path (Path.GoLeft :: path2) ->
                    Path path2

                Path (Path.GoRight :: path2) ->
                    Outside

                Inside ->
                    Inside

                Outside ->
                    Outside
    in
        { c | precedence = i, focusPosition = focusPosition, assocPosition = assocPosition }


contextInRight : Int -> Context -> Context
contextInRight i c =
    let
        focusPosition =
            case c.focusPosition of
                Path [] ->
                    Inside

                Path (Path.GoLeft :: path2) ->
                    Outside

                Path (Path.GoRight :: path2) ->
                    Path path2

                Inside ->
                    Inside

                Outside ->
                    Outside

        assocPosition =
            case c.assocPosition of
                Path [] ->
                    Inside

                Path (Path.GoLeft :: path2) ->
                    Outside

                Path (Path.GoRight :: path2) ->
                    Path path2

                Inside ->
                    Inside

                Outside ->
                    Outside
    in
        { c | precedence = i, focusPosition = focusPosition, assocPosition = assocPosition }


binaryShape : Context -> Int -> Term -> Shape -> Term -> Shape
binaryShape ctx i t1 g t2 =
    let
        s =
            reallyBinaryShape
                (reallyTermShape (contextInLeft (i + 1) ctx) t1)
                g
                (reallyTermShape (contextInRight (i + 1) ctx) t2)
    in
    if i < ctx.precedence then
        parens ctx s
    else
        s


reallyTermShape : Context -> Term -> Shape
reallyTermShape ctx term =
    let
        shape3 =
            if contextIsMathML ctx then
                shape2
            else
                { shape2 | mathML = [] }

        shape2 =
            shape1

        shape1 =
            if contextIsTrueFocus ctx then
                { shape0
                    | focus = Just { x = 0, y = 250 - lineHeight, width = shape0.width, height = lineHeight * 2 }
                    }
            else
                shape0

        shape0 =
            reallyTermShape2 ctx term
    in
    shape3


reallyTermShape2 : Context -> Term -> Shape
reallyTermShape2 ctx term =
    case term of
        Binary Equivalence t1 t2 ->
            binaryShape ctx 0 t1 (leftRightArrowShape ctx) t2

        Binary Implication t3 t4 ->
            binaryShape ctx 0 t3 (implicationArrowShape ctx) t4

        Binary Disjunction t5 t6 ->
            binaryShape ctx 0 t5 (disjunctionShape ctx) t6

        Binary Conjunction t7 t8 ->
            binaryShape ctx 0 t7 (conjunctionShape ctx) t8

        Binary EqualTo t9 t10 ->
            binaryShape ctx 1 t9 (equalShape ctx) t10

        Binary NotEqualTo t11 t12 ->
            binaryShape ctx 1 t11 (notEqualShape ctx) t12

        Binary Addition t13 t14 ->
            binaryShape ctx 2 t13 (plusShape ctx) t14

        Binary Subtraction t15 t16 ->
            binaryShape ctx 2 t15 (minusShape ctx) t16

        Binary Multiplication t17 t18 ->
            let
                s =
                    apposition (reallyTermShape (contextInLeft 5 ctx) t17) (reallyTermShape (contextInRight 6 ctx) t18)
            in
            if 4 < ctx.precedence then
                parens ctx s
            else
                s

        Binary Division t19 t20 ->
            let
                s =
                    fractionShape ctx (reallyTermShape (contextInLeft 2 ctx) t19) (reallyTermShape (contextInRight 2 ctx) t20)
            in
            if 4 < ctx.precedence then
                parens ctx s
            else
                s

        Unary IsReal t21 ->
            let
                s =
                    reallyBinaryShape
                        (reallyTermShape (contextInLeft 5 ctx) t21)
                        (inShape ctx)
                        (reallyGlyphShape
                             (-40)
                             0
                             (contextColor ctx)
                             (MathML.mi [ MathML.Attributes.mathvariant "blackboard" ] [ H.text "R" ])
                             blackboardRGlyph
                        )
            in
            if 4 < ctx.precedence then
                parens ctx s
            else
                s

        Unary IsNotReal t22 ->
            let
                s =
                    reallyBinaryShape
                        (reallyTermShape (contextInLeft 5 ctx) t22)
                        (notInShape ctx)
                        (reallyGlyphShape
                             (-40)
                             0
                             (contextColor ctx)
                             (MathML.mi [ MathML.Attributes.mathvariant "blackboard" ] [ H.text "R" ])
                             blackboardRGlyph
                        )
            in
            if 4 < ctx.precedence then
                parens ctx s
            else
                s

        Unary Not t23 ->
            unaryShape ctx 1 (notShape ctx) t23

        Unary Square (Atom (Num n1)) ->
            let
                s =
                    superscriptShape s2 s1

                s1 =
                    if ctx.precedence == 6 then
                        reallyTermShape (contextInLeft 6 ctx) (Atom (Num n1))
                    else
                        reallyTermShape (contextInLeft 5 ctx) (Atom (Num n1))

                s2 =
                    reallyTermShape (contextInRight 0 ctx) (Atom (Num Nat.two))
            in
            if 6 < ctx.precedence then
                parens ctx s
            else
                s

        Unary Square t24 ->
            let
                s =
                    superscriptShape s2 s1

                s1 =
                    reallyTermShape (contextInLeft 7 ctx) t24

                s2 =
                    reallyTermShape (contextInRight 0 ctx) (Atom (Num Nat.two))
            in
            if 6 < ctx.precedence then
                parens ctx s
            else
                s

        Unary Neg t25 ->
            unaryShape ctx 3 (minusShape ctx) t25

        Atom Top ->
            downTackShape ctx

        Atom Bot ->
            upTackShape ctx

        Atom VarA ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "A" ]) latinUpperAGlyph

        Atom VarB ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "B" ]) latinUpperBGlyph

        Atom VarC ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "C" ]) latinUpperCGlyph

        Atom AlgA ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "a" ]) latinLowerAGlyph

        Atom AlgB ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "b" ]) latinLowerBGlyph

        Atom AlgC ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "c" ]) latinLowerCGlyph

        Atom AlgX ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "x" ]) latinLowerXGlyph

        Atom AlgY ->
            reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "y" ]) latinLowerYGlyph

        Atom AlgA1 ->
            subscriptShape
                (intShape ctx 1)
                (reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "a" ]) latinLowerAGlyph)

        Atom AlgB1 ->
            subscriptShape
                (intShape ctx 1)
                (reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "b" ]) latinLowerBGlyph)

        Atom AlgC1 ->
            subscriptShape
                (intShape ctx 1)
                (reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "c" ]) latinLowerCGlyph)

        Atom AlgA2 ->
            subscriptShape
                (intShape ctx 2)
                (reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "a" ]) latinLowerAGlyph)

        Atom AlgB2 ->
            subscriptShape
                (intShape ctx 2)
                (reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "b" ]) latinLowerBGlyph)

        Atom AlgC2 ->
            subscriptShape
                (intShape ctx 2)
                (reallyGlyphShape (-40) 0 (contextColor ctx) (MathML.mi [] [ H.text "c" ]) latinLowerCGlyph)

        Atom (MetaVar MetaA) ->
            glyphShape metaColor (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "A" ]) frakturAGlyph

        Atom (MetaVar MetaB) ->
            glyphShape metaColor (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "B" ]) frakturBGlyph

        Atom (MetaVar MetaC) ->
            glyphShape metaColor (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "C" ]) frakturCGlyph

        Atom (MetaVar MetaAlgA) ->
            glyphShape
                metaColor
                (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "a" ])
                frakturLowerAGlyph

        Atom (MetaVar MetaAlgB) ->
            glyphShape
                metaColor
                (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "b" ])
                frakturLowerBGlyph

        Atom (MetaVar MetaAlgC) ->
            glyphShape
                metaColor
                (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "c" ])
                frakturLowerCGlyph

        Atom (MetaVar MetaAlgD) ->
            glyphShape
                metaColor
                (MathML.mi [ MathML.Attributes.mathvariant "fraktur" ] [ H.text "d" ])
                frakturLowerDGlyph

        Atom (Num n2) ->
            let
                s =
                    List.foldr horizontal emptyShape (List.map (intShape ctx) (Nat.digits n2))
            in
            if 5 < ctx.precedence then
                parens ctx s
            else
                s


unaryShape : Context -> Int -> Shape -> Term -> Shape
unaryShape ctx i g t =
    let
        s =
            reallyUnaryShape g (reallyTermShape (contextInRight (i + 1) ctx) t)
    in
    if i < ctx.precedence then
        parens ctx s
    else
        s


ruleShape : Rule -> Shape
ruleShape rule =
    let
        antecedents =
            if List.isEmpty rule.antecedents then
                emptyShape
            else
                horizontal (commaSep (List.map (reallyTermShape { precedence = 0, focusPosition = Inside, assocPosition = Outside }) rule.antecedents)) space

        left =
            reallyTermShape { precedence = 0, focusPosition = Inside, assocPosition = Outside } rule.left

        right =
            reallyTermShape { precedence = 0, focusPosition = Inside, assocPosition = Outside } rule.right
    in
        List.foldr horizontal emptyShape [ antecedents, turnstileShape, space, left, space, longLeftRightArrowShape, space, right ]


midRuleShape : Rule -> Shape
midRuleShape rule =
    let
        shape =
            ruleShape rule
    in
        { shape | above = midline + 750, below = 750 - midline }


{-| Aligns center-left with a standard height.
-}
leftRuleDiv : Rule -> Html msg
leftRuleDiv =
    lazy (midRuleShape >> expandShape >> leftDiv)


ruleSpan : Rule -> Html a
ruleSpan =
    lazy (ruleShape >> shapeSpan)


termShape : Term -> Shape
termShape =
    reallyTermShape { precedence = 0, focusPosition = Inside, assocPosition = Outside }


midTermShape : Term -> Shape
midTermShape term =
    let
        shape =
            termShape term
    in
        { shape | above = midline + 750, below = 750 - midline }


formulaView : Term -> Html msg
formulaView =
    midTermShape >> formulaShapeView


leftTermDiv : Term -> Html msg
leftTermDiv =
    lazy (midTermShape >> expandShape >> leftDiv)


centerTermDiv : Term -> Html msg
centerTermDiv =
    lazy (midTermShape >> expandShape >> centerDiv)


termSpan : Term -> Html a
termSpan =
    lazy (termShape >> shapeSpan)


yellowTermShape : Term -> Shape
yellowTermShape term =
    let
        shape =
            reallyTermShape { precedence = 0, focusPosition = Outside, assocPosition = Inside } term
    in
        { shape | above = midline + 750, below = 750 - midline }


yellowLeftTermDiv : Term -> Html msg
yellowLeftTermDiv =
    lazy (yellowTermShape >> expandShape >> leftDiv)


reallyFormulaHtml1 : Term -> Path -> Html msg
reallyFormulaHtml1 term path =
    term
        |> reallyTermShape { precedence = 0, focusPosition = Path path, assocPosition = Outside }
        |> formulaShapeView


reallyFormulaHtml0 : Term -> Html msg
reallyFormulaHtml0 term =
    reallyFormulaHtml1 term []


formulaHtml0 : Term -> Html msg
formulaHtml0 =
    lazy reallyFormulaHtml0


formulaHtml1 : Term -> Path -> Html msg
formulaHtml1 =
    lazy2 reallyFormulaHtml1


reallyFormulaHtml2 : Term -> Path -> Path -> Html msg
reallyFormulaHtml2 term path1 path2 =
    case path2 of
        [] ->
            term
                |> reallyTermShape { precedence = 0, focusPosition = Path path1, assocPosition = Outside }
                |> formulaShapeView

        _ ->
            term
                |> reallyTermShape { precedence = 0, focusPosition = Path path1, assocPosition = Path path2 }
                |> formulaShapeView


formulaHtml2 : Term -> Path -> Path -> Html msg
formulaHtml2 =
    lazy3 reallyFormulaHtml2
