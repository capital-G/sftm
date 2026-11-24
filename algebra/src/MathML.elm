module MathML exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


mo : List (Attribute msg) -> List (Html msg) -> Html msg
mo =
    node "mo"


mi : List (Attribute msg) -> List (Html msg) -> Html msg
mi =
    node "mi"


math : List (Attribute msg) -> List (Html msg) -> Html msg
math attrs =
    node "math" (attribute "xmlns" "http://www.w3.org/1998/Math/MathML" :: attrs)
