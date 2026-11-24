module Rat exposing (..)

import Nat exposing (Nat)
import Json.Decode
import Json.Encode


type Rat
    = Pos Nat Nat
    | Neg Nat Nat


toDenominatorString : Rat -> String
toDenominatorString r =
    case r of
        Pos n1 _ ->
            Nat.toString n1

        Neg n2 _ ->
            Nat.toString n2


toNumeratorString : Rat -> String
toNumeratorString r =
    case r of
        Pos n1 _ ->
            Nat.toString n1

        Neg n2 _ ->
            Nat.toString n2


toDenominator : Rat -> Nat
toDenominator r =
    case r of
        Pos _ n1 ->
            n1

        Neg _ n2 ->
            n2


toNumerator : Rat -> Nat
toNumerator r =
    case r of
        Pos n1 _ ->
            n1

        Neg n2 _ ->
            n2


toString : Rat -> String
toString r =
    case r of
        Pos n1 [ 1 ] ->
            Nat.toString n1

        Pos n2 n3 ->
            Nat.toString n2 ++ "/" ++ Nat.toString n3

        Neg n4 [ 1 ] ->
            "-" ++ Nat.toString n4

        Neg n5 n6 ->
            "-" ++ Nat.toString n5 ++ "/" ++ Nat.toString n6


encoder : Rat -> Json.Encode.Value
encoder =
    Json.Encode.string << toString


digits : Rat -> Nat
digits r =
    case r of
        Pos n1 _ ->
            Nat.digits n1

        Neg n2 _ ->
            Nat.digits n2


isInt : Rat -> Bool
isInt r =
    case r of
        Pos _ n1 ->
            Nat.eq n1 (Nat.fromInt 1)

        Neg _ n2 ->
            Nat.eq n2 (Nat.fromInt 1)


isNegative : Rat -> Bool
isNegative r =
    case r of
        Pos _ _ ->
            False

        Neg _ _ ->
            True


nan : Rat
nan =
    Pos (Nat.fromInt 1) (Nat.fromInt 0)


isNan : Rat -> Bool
isNan r =
    r == nan


isEqual : Rat -> Rat -> Bool
isEqual r1 r2 =
    not (isNan r1) && not (isNan r2) && r1 == r2


isNotEqual : Rat -> Rat -> Bool
isNotEqual r1 r2 =
    isNan r1 || isNan r2 || not (r1 == r2)


fromInt n =
    if n < 0 then
        Neg (Nat.fromInt (0 - n)) (Nat.fromInt 1)
    else
        Pos (Nat.fromInt n) (Nat.fromInt 1)


zero : Rat
zero =
    fromInt 0


one : Rat
one =
    fromInt 1


two : Rat
two =
    fromInt 2


three : Rat
three =
    fromInt 3


four : Rat
four =
    fromInt 4


five : Rat
five =
    fromInt 5


six : Rat
six =
    fromInt 6


normalize : Rat -> Rat
normalize r =
    case r of
        Pos n2 n1 ->
            if Nat.eq n1 (Nat.fromInt 0) then
                nan
            else if Nat.eq n2 (Nat.fromInt 0) then
                zero
            else
                let
                    n =
                        Nat.gcd n2 n1
                in
                Pos (Nat.div n2 n) (Nat.div n1 n)

        Neg n4 n3 ->
            if Nat.eq n3 (Nat.fromInt 0) then
                nan
            else if Nat.eq n4 (Nat.fromInt 0) then
                zero
            else
                let
                    n =
                        Nat.gcd n4 n3
                in
                Neg (Nat.div n4 n) (Nat.div n3 n)


over n1 n2 =
    if n1 < 0 then
        if n2 < 0 then
            normalize (Pos (Nat.fromInt (0 - n1)) (Nat.fromInt (0 - n2)))
        else
            normalize (Neg (Nat.fromInt (0 - n1)) (Nat.fromInt n2))
    else if n2 < 0 then
        normalize (Neg (Nat.fromInt n1) (Nat.fromInt (0 - n2)))
    else
        normalize (Pos (Nat.fromInt n1) (Nat.fromInt n2))


isZero : Rat -> Bool
isZero r =
    r == over 0 1


inverse : Rat -> Rat
inverse r =
    case r of
        Pos n2 n1 ->
            normalize (Pos n1 n2)

        Neg n4 n3 ->
            normalize (Neg n3 n4)


add : Rat -> Rat -> Rat
add r1 r2 =
    case r1 of
        Pos n2 n1 ->
            case r2 of
                Pos n4 n3 ->
                    let
                        a11 =
                            Nat.mul n2 n3

                        a21 =
                            Nat.mul n4 n1
                    in
                    normalize (Pos (Nat.add a11 a21) (Nat.mul n1 n3))

                Neg n6 n5 ->
                    let
                        a11 =
                            Nat.mul n2 n5

                        a21 =
                            Nat.mul n6 n1
                    in
                    if Nat.lt a11 a21 then
                        normalize (Neg (Nat.sub a21 a11) (Nat.mul n1 n5))
                    else
                        normalize (Pos (Nat.sub a11 a21) (Nat.mul n1 n5))

        Neg n8 n7 ->
            case r2 of
                Pos n10 n9 ->
                    let
                        a11 =
                            Nat.mul n8 n9

                        a21 =
                            Nat.mul n10 n7
                    in
                    if Nat.lt a11 a21 then
                        normalize (Pos (Nat.sub a21 a11) (Nat.mul n7 n9))
                    else
                        normalize (Neg (Nat.sub a11 a21) (Nat.mul n7 n9))

                Neg n12 n11 ->
                    let
                        a11 =
                            Nat.mul n8 n11

                        a21 =
                            Nat.mul n12 n7
                    in
                    normalize (Neg (Nat.add a11 a21) (Nat.mul n7 n11))


multiply : Rat -> Rat -> Rat
multiply r1 r2 =
    case r1 of
        Pos n2 n1 ->
            case r2 of
                Pos n3 n4 ->
                    normalize (Pos (Nat.mul n2 n3) (Nat.mul n1 n4))

                Neg n5 n6 ->
                    normalize (Neg (Nat.mul n2 n5) (Nat.mul n1 n6))

        Neg n8 n7 ->
            case r2 of
                Pos n9 n10 ->
                    normalize (Neg (Nat.mul n8 n9) (Nat.mul n7 n10))

                Neg n11 n12 ->
                    normalize (Pos (Nat.mul n8 n11) (Nat.mul n7 n12))


divide : Rat -> Rat -> Rat
divide r1 r2 =
    multiply r1 (inverse r2)


negate : Rat -> Rat
negate r =
    case r of
        Pos n1 n2 ->
            Neg n1 n2

        Neg n3 n4 ->
            Pos n3 n4


subtract : Rat -> Rat -> Rat
subtract r1 r2 =
    add r1 (negate r2)


fromString : String -> Rat
fromString s =
    case String.split "/" s of
        [ x2, x1 ] ->
            case String.uncons x2 of
                Just ( '-', b1 ) ->
                    Neg (Nat.fromString b1) (Nat.fromString x1)

                _ ->
                    Pos (Nat.fromString x2) (Nat.fromString x1)

        [ x3 ] ->
            case String.uncons x3 of
                Just ( '-', b2 ) ->
                    Neg (Nat.fromString b2) (Nat.fromInt 1)

                _ ->
                    Pos (Nat.fromString x3) (Nat.fromInt 1)

        _ ->
            Debug.crash "fromString"


decoder : Json.Decode.Decoder Rat
decoder =
    Json.Decode.map fromString Json.Decode.string


fromNat : Nat -> Rat
fromNat n =
    Pos n (Nat.fromInt 1)
