module Utilities exposing (..)

import Html exposing (Html, Attribute)
import Html.Events exposing (on)
import Html.Attributes exposing (attribute)
import Json.Decode exposing (succeed)
import Task exposing (attempt)
import Dom.Scroll exposing (toTop, toBottom)


gcd : Int -> Int -> Int
gcd a b =
    let
        gcd_ a b =
            if b == 0 then
                a
            else
                gcd_ b (a % b)
    in
    gcd_ (abs a) (abs b)


ariaHidden : Bool -> Attribute msg
ariaHidden b =
    if b then
        attribute "aria-hidden" "true"
    else
        attribute "aria-hidden" "false"


unsafeTail : List a -> List a
unsafeTail xs =
    case xs of
        [] ->
            Debug.crash "impossible"

        _ :: r ->
            r


scrollToBottomCmd : msg -> Cmd msg
scrollToBottomCmd msg =
    attempt (\result -> msg) (toBottom "scrolling")


scrollToTopCmd : msg -> Cmd msg
scrollToTopCmd msg =
    attempt (\result -> msg) (toTop "scrolling")


onScroll : msg -> Attribute msg
onScroll f =
    on "scroll" (succeed f)


remove : Int -> List a -> List a
remove n xs =
    List.take n xs ++ List.drop (n + 1) xs


fromJust : Maybe a -> a
fromJust m =
    case m of
        Nothing ->
            Debug.crash "impossible"

        Just x ->
            x


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


set : Int -> a -> List a -> List a
set i x xs =
    List.append (List.take i xs) (x :: List.drop (i + 1) xs)


unsafeGet : Int -> List a -> a
unsafeGet i xs =
    case List.head (List.drop i xs) of
        Nothing ->
            Debug.crash "impossible"

        Just x ->
            x


last : List a -> a
last =
    unsafeGet 0 << List.reverse


get : Int -> List a -> Maybe a
get i xs =
    List.head (List.drop i xs)


castHtml : Html Never -> Html a
castHtml =
    Html.map never
