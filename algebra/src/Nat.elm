module Nat exposing (..)

import Char
import Json.Decode
import Json.Encode


{-| Arbitrary-precision natural numbers, stored as a list of decimal digits
(least significant digit first).
-}
type alias Nat =
    List Int


digits : List number -> List number
digits n =
    if List.isEmpty n then
        [ 0 ]
    else
        List.reverse n


reallyLt : Bool -> List comparable -> List comparable -> Bool
reallyLt r a b =
    case a of
        [] ->
            not (List.isEmpty b) || r

        x1 :: rest ->
            case b of
                [] ->
                    False

                x2 :: rest1 ->
                    reallyLt (x1 < x2 || x1 == x2 && r) rest rest1


lte : List comparable -> List comparable -> Bool
lte =
    reallyLt True


eq : List a -> List a -> Bool
eq a b =
    case a of
        [] ->
            List.isEmpty b

        x1 :: rest ->
            case b of
                [] ->
                    False

                x2 :: rest1 ->
                    x1 == x2 && eq rest rest1


lt =
    reallyLt False


removeZeroes : List number -> List number1
removeZeroes a =
    case a of
        0 :: rest ->
            removeZeroes rest

        _ ->
            a


reallySubDigit : List number -> number1 -> List number -> List number
reallySubDigit r c a =
    case a of
        [] ->
            List.reverse (removeZeroes r)

        x1 :: rest ->
            if c == 0 then
                List.foldl (\x y -> x :: y) a r
            else
                let
                    n =
                        x1 - c
                in
                if n < 0 then
                    reallySubDigit (10 + n :: r) 1 rest
                else
                    reallySubDigit (n :: r) 0 rest


reallySub r c a b =
    case a of
        [] ->
            List.reverse (removeZeroes r)

        x1 :: rest ->
            case b of
                [] ->
                    reallySubDigit r c a

                x2 :: rest1 ->
                    let
                        n =
                            x1 - x2 - c
                    in
                    if n < 0 then
                        reallySub (10 + n :: r) 1 rest rest1
                    else
                        reallySub (n :: r) 0 rest rest1


sub : List number -> List number -> List number
sub a b =
    reallySub [] 0 a b


reallyAddDigit : List number -> number1 -> List number -> List number
reallyAddDigit r c a =
    if c == 0 then
        List.append (List.reverse r) a
    else
        case a of
            [] ->
                List.reverse (c :: r)

            x1 :: rest ->
                let
                    n =
                        x1 + c
                in
                if n > 9 then
                    reallyAddDigit (n - 10 :: r) 1 rest
                else
                    reallyAddDigit (n :: r) 0 rest


reallyAdd r c a b =
    case a of
        [] ->
            reallyAddDigit r c b

        x1 :: rest ->
            case b of
                [] ->
                    reallyAddDigit r c a

                x2 :: rest1 ->
                    let
                        n =
                            x1 + x2 + c
                    in
                    if n > 9 then
                        reallyAdd (n - 10 :: r) 1 rest rest1
                    else
                        reallyAdd (n :: r) 0 rest rest1


add : List number -> List number -> List number
add =
    reallyAdd [] 0


reallyMul : List number -> List number -> List number1 -> List number
reallyMul r a b =
    case b of
        [] ->
            r

        _ :: _ ->
            reallyMul (add r a) a (sub b [ 1 ])


mul : List number -> List number1 -> List number
mul a b =
    reallyMul [] a b


divMod a b =
    if lt a b then
        ( [], a )
    else
        case divMod (sub a b) b of
            ( q, r ) ->
                ( add q [ 1 ], r )


div : List number -> List number -> List number1
div a b =
    case divMod a b of
        ( r, _ ) ->
            r


mod : List number -> List number -> List number
mod a b =
    case divMod a b of
        ( _, r ) ->
            r


reallyGcd : List number -> List number -> List number
reallyGcd a b =
    if eq b [] then
        a
    else
        reallyGcd b (mod a b)


gcd a b =
    if lt a b then
        reallyGcd b a
    else
        reallyGcd a b


fromString : String -> Nat
fromString p23 =
    (\s ->
         if s == [ 0 ] then
             []
         else
             s
    )
        (List.reverse (List.map (\c -> c - Char.toCode '0') (List.map Char.toCode (String.toList p23))))


decoder : Json.Decode.Decoder (Nat)
decoder =
    Json.Decode.map fromString Json.Decode.string


toString : List a -> String
toString a =
    if List.isEmpty a then
        "0"
    else
        String.concat (List.map Basics.toString (List.reverse a))


toStrings : ( List a, List a1 ) -> ( String, String )
toStrings ( a1, b1 ) =
    ( toString a1, toString b1 )


encoder : List a -> Json.Encode.Value
encoder =
    Json.Encode.string << toString


toInt : List number -> number
toInt =
    List.foldr (\d i -> i * 10 + d) 0


reallyFromInt : Nat -> Int -> Nat
reallyFromInt r i =
    if i == 0 then
        List.reverse r
    else
        reallyFromInt (i % 10 :: r) (i // 10)


fromInt : Int -> Nat
fromInt =
    reallyFromInt []


six : Nat
six =
    fromInt 6


five : Nat
five =
    fromInt 5


four : Nat
four =
    fromInt 4


three : Nat
three =
    fromInt 3


two : Nat
two =
    fromInt 2


one : Nat
one =
    fromInt 1


zero : Nat
zero =
    fromInt 0
