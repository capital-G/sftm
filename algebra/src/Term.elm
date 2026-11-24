module Term exposing (..)

import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import Nat exposing (Nat)
import Rat exposing (Rat)


type Term
    = Atom Atom
    | Unary Unary Term
    | Binary Binary Term Term


type Binary
    = Conjunction
    | Disjunction
    | Implication
    | Equivalence
    | EqualTo
    | NotEqualTo
    | Addition
    | Subtraction
    | Multiplication
    | Division


type Unary
    = Not
    | Neg
    | IsReal
    | IsNotReal
    | Square


type Atom
    = Top
    | Bot
    | VarA
    | VarB
    | VarC
    | AlgX
    | AlgY
    | AlgA
    | AlgB
    | AlgC
    | AlgA1
    | AlgB1
    | AlgC1
    | AlgA2
    | AlgB2
    | AlgC2
    | Num Nat
    | MetaVar MetaVar


type MetaVar
    = MetaA
    | MetaB
    | MetaC
    | MetaAlgA
    | MetaAlgB
    | MetaAlgC
    | MetaAlgD


variableIndex : Term -> number
variableIndex t =
    case t of
        Atom AlgA ->
            0

        Atom AlgB ->
            1

        Atom AlgC ->
            2

        Atom AlgX ->
            3

        Atom AlgY ->
            4

        Atom AlgA1 ->
            5

        Atom AlgB1 ->
            6

        Atom AlgC1 ->
            7

        Atom AlgA2 ->
            8

        Atom AlgB2 ->
            9

        Atom AlgC2 ->
            10

        _ ->
            Debug.crash "impossible"


uniqueAddVariable : Term -> List Term -> List Term
uniqueAddVariable t ts =
    case ts of
        [] ->
            [ t ]

        x1 :: rest ->
            if variableIndex t == variableIndex x1 then
                x1 :: rest
            else if variableIndex t < variableIndex x1 then
                t :: x1 :: rest
            else
                x1 :: uniqueAddVariable t rest


uniqueSortVariables : List Term -> List Term
uniqueSortVariables ts =
    List.foldl uniqueAddVariable [] ts


parens : Int -> Int -> String -> String
parens i j s =
    if 0 < j then
        "(" ++ s ++ ")"
    else
        s


reallyTex : Int -> Term -> String
reallyTex p term =
    case term of
        Atom Top ->
            "\\top"

        Atom Bot ->
            "\\bot"

        Atom VarA ->
            "A"

        Atom VarB ->
            "B"

        Atom VarC ->
            "C"

        Atom AlgX ->
            "x"

        Atom AlgY ->
            "y"

        Atom (MetaVar MetaA) ->
            "\\frak{A}"

        Atom (MetaVar MetaB) ->
            "\\frak{B}"

        Atom (MetaVar MetaC) ->
            "\\frak{C}"

        Atom (Num n1) ->
            Nat.toString n1

        Unary Not t1 ->
            parens 5 p ("\\lnot " ++ reallyTex 5 t1)

        Binary Addition t2 t3 ->
            parens 1 p (reallyTex 1 t2 ++ " + " ++ reallyTex 1 t3)

        Binary EqualTo t4 t5 ->
            parens 1 p (reallyTex 1 t4 ++ " = " ++ reallyTex 1 t5)

        Binary Conjunction t6 t7 ->
            parens 4 p (reallyTex 4 t6 ++ " \\land " ++ reallyTex 4 t7)

        Binary Disjunction t8 t9 ->
            parens 3 p (reallyTex 3 t8 ++ " \\lor " ++ reallyTex 3 t9)

        Binary Implication t10 t11 ->
            parens 2 p (reallyTex 2 t10 ++ " \\to " ++ reallyTex 2 t11)

        Binary Equivalence t12 t13 ->
            parens 1 p (reallyTex 1 t12 ++ " \\leftrightarrow " ++ reallyTex 1 t13)

        _ ->
            Debug.crash "todo"


tex : Term -> String
tex term =
    "$" ++ reallyTex 0 term ++ "$"


{-| Makes sure every metavariable in the term is in the dictionary, adding an
empty entry if needed.
-}
addMetavariables : Term -> List ( MetaVar, List Term ) -> List ( MetaVar, List Term )
addMetavariables term dict =
    let
        f a1 d =
            case d of
                [] ->
                    [ ( a1, [] ) ]

                ( a2, xs ) :: ds ->
                    if a1 == a2 then
                        ( a2, xs ) :: ds
                    else
                        ( a2, xs ) :: f a1 ds
    in
        case term of
            Atom (MetaVar m) ->
                f m dict

            Atom _ ->
                dict

            Unary _ t ->
                addMetavariables t dict

            Binary _ t1 t2 ->
                addMetavariables t2 (addMetavariables t1 dict)


encoder : Term -> Value
encoder term =
    case term of
        Atom Top ->
            Json.Encode.string "top"

        Atom Bot ->
            Json.Encode.string "bot"

        Atom VarA ->
            Json.Encode.string "var-upper-a"

        Atom VarB ->
            Json.Encode.string "var-upper-b"

        Atom VarC ->
            Json.Encode.string "var-upper-c"

        Atom AlgA ->
            Json.Encode.string "var-lower-a"

        Atom AlgB ->
            Json.Encode.string "var-lower-b"

        Atom AlgC ->
            Json.Encode.string "var-lower-c"

        Atom AlgX ->
            Json.Encode.string "var-lower-x"

        Atom AlgY ->
            Json.Encode.string "var-lower-y"

        Atom AlgA1 ->
            Json.Encode.string "var-lower-a1"

        Atom AlgB1 ->
            Json.Encode.string "var-lower-b1"

        Atom AlgC1 ->
            Json.Encode.string "var-lower-c1"

        Atom AlgA2 ->
            Json.Encode.string "var-lower-a2"

        Atom AlgB2 ->
            Json.Encode.string "var-lower-b2"

        Atom AlgC2 ->
            Json.Encode.string "var-lower-c2"

        Atom (MetaVar MetaA) ->
            Json.Encode.string "meta-upper-a"

        Atom (MetaVar MetaB) ->
            Json.Encode.string "meta-upper-b"

        Atom (MetaVar MetaC) ->
            Json.Encode.string "meta-upper-c"

        Atom (MetaVar MetaAlgA) ->
            Json.Encode.string "meta-lower-a"

        Atom (MetaVar MetaAlgB) ->
            Json.Encode.string "meta-lower-b"

        Atom (MetaVar MetaAlgC) ->
            Json.Encode.string "meta-lower-c"

        Atom (MetaVar MetaAlgD) ->
            Json.Encode.string "meta-lower-d"

        Atom (Num n1) ->
            Json.Encode.list [ Json.Encode.string "number", Nat.encoder n1 ]

        Unary Not t1 ->
            Json.Encode.list [ Json.Encode.string "not", encoder t1 ]

        Unary Neg t2 ->
            Json.Encode.list [ Json.Encode.string "neg", encoder t2 ]

        Unary Square t3 ->
            Json.Encode.list [ Json.Encode.string "square", encoder t3 ]

        Unary _ _ ->
            Debug.crash "todo"

        Binary Addition t4 t5 ->
            Json.Encode.list [ Json.Encode.string "add", encoder t4, encoder t5 ]

        Binary Subtraction t6 t7 ->
            Json.Encode.list [ Json.Encode.string "subtract", encoder t6, encoder t7 ]

        Binary Multiplication t8 t9 ->
            Json.Encode.list [ Json.Encode.string "multiply", encoder t8, encoder t9 ]

        Binary Division t10 t11 ->
            Json.Encode.list [ Json.Encode.string "divide", encoder t10, encoder t11 ]

        Binary EqualTo t12 t13 ->
            Json.Encode.list [ Json.Encode.string "equal-to", encoder t12, encoder t13 ]

        Binary NotEqualTo t14 t15 ->
            Json.Encode.list [ Json.Encode.string "not-equal-to", encoder t14, encoder t15 ]

        Binary Conjunction t16 t17 ->
            Json.Encode.list [ Json.Encode.string "and", encoder t16, encoder t17 ]

        Binary Disjunction t18 t19 ->
            Json.Encode.list [ Json.Encode.string "or", encoder t18, encoder t19 ]

        Binary Implication t20 t21 ->
            Json.Encode.list [ Json.Encode.string "implies", encoder t20, encoder t21 ]

        Binary Equivalence t22 t23 ->
            Json.Encode.list [ Json.Encode.string "equivalent", encoder t22, encoder t23 ]


performArithmetic : Term -> Rat
performArithmetic t =
    case t of
        Atom (Num n1) ->
            Rat.fromNat n1

        Unary Neg t1 ->
            Rat.negate (performArithmetic t1)

        Unary Square t2 ->
            Rat.multiply (performArithmetic t2) (performArithmetic t2)

        Binary Addition t3 t4 ->
            Rat.add (performArithmetic t3) (performArithmetic t4)

        Binary Subtraction t5 t6 ->
            Rat.subtract (performArithmetic t5) (performArithmetic t6)

        Binary Multiplication t7 t8 ->
            Rat.multiply (performArithmetic t7) (performArithmetic t8)

        Binary Division t9 t10 ->
            Rat.divide (performArithmetic t9) (performArithmetic t10)

        _ ->
            Debug.crash "impossible"


isArithmetic : Term -> Bool
isArithmetic t =
    case t of
        Atom (Num _) ->
            True

        Unary Neg t1 ->
            isArithmetic t1

        Unary Square t2 ->
            isArithmetic t2

        Binary Addition t3 t4 ->
            isArithmetic t3 && isArithmetic t4

        Binary Subtraction t5 t6 ->
            isArithmetic t5 && isArithmetic t6

        Binary Multiplication t7 t8 ->
            isArithmetic t7 && isArithmetic t8

        Binary Division t9 t10 ->
            isArithmetic t9 && isArithmetic t10

        _ ->
            False


reducesToEqualTo : Term -> Term -> Bool
reducesToEqualTo t1 t2 =
    isArithmetic t1 && isArithmetic t2 && Rat.isEqual (performArithmetic t1) (performArithmetic t2)


reducesToNotEqualTo : Term -> Term -> Bool
reducesToNotEqualTo t1 t2 =
    isArithmetic t1 && isArithmetic t2 && Rat.isNotEqual (performArithmetic t1) (performArithmetic t2)


hasAlgebraicSubterms : Term -> Bool
hasAlgebraicSubterms t =
    case t of
        Atom AlgA ->
            True

        Atom AlgB ->
            True

        Atom AlgC ->
            True

        Atom AlgX ->
            True

        Atom AlgY ->
            True

        Atom AlgA1 ->
            True

        Atom AlgB1 ->
            True

        Atom AlgC1 ->
            True

        Atom AlgA2 ->
            True

        Atom AlgB2 ->
            True

        Atom AlgC2 ->
            True

        Atom (Num _) ->
            True

        Atom (MetaVar MetaAlgA) ->
            True

        Atom (MetaVar MetaAlgB) ->
            True

        Atom (MetaVar MetaAlgC) ->
            True

        Atom (MetaVar MetaAlgD) ->
            True

        Unary Neg _ ->
            True

        Unary Square _ ->
            True

        Binary Addition _ _ ->
            True

        Binary Subtraction _ _ ->
            True

        Binary Multiplication _ _ ->
            True

        Binary Division _ _ ->
            True

        Binary EqualTo _ _ ->
            True

        Binary NotEqualTo _ _ ->
            True

        _ ->
            False


isAlgebraic : Term -> Bool
isAlgebraic t =
    case t of
        Atom AlgA ->
            True

        Atom AlgB ->
            True

        Atom AlgC ->
            True

        Atom AlgX ->
            True

        Atom AlgY ->
            True

        Atom AlgA1 ->
            True

        Atom AlgB1 ->
            True

        Atom AlgC1 ->
            True

        Atom AlgA2 ->
            True

        Atom AlgB2 ->
            True

        Atom AlgC2 ->
            True

        Atom (Num _) ->
            True

        Atom (MetaVar MetaAlgA) ->
            True

        Atom (MetaVar MetaAlgB) ->
            True

        Atom (MetaVar MetaAlgC) ->
            True

        Atom (MetaVar MetaAlgD) ->
            True

        Unary Neg _ ->
            True

        Unary Square _ ->
            True

        Binary Addition _ _ ->
            True

        Binary Subtraction _ _ ->
            True

        Binary Multiplication _ _ ->
            True

        Binary Division _ _ ->
            True

        _ ->
            False


isLogical : Term -> Bool
isLogical t =
    case t of
        Atom Top ->
            True

        Atom Bot ->
            True

        Atom VarA ->
            True

        Atom VarB ->
            True

        Atom VarC ->
            True

        Atom (MetaVar MetaA) ->
            True

        Atom (MetaVar MetaB) ->
            True

        Atom (MetaVar MetaC) ->
            True

        Unary Not _ ->
            True

        Binary EqualTo _ _ ->
            True

        Binary NotEqualTo _ _ ->
            True

        Binary Conjunction _ _ ->
            True

        Binary Disjunction _ _ ->
            True

        Binary Implication _ _ ->
            True

        Binary Equivalence _ _ ->
            True

        _ ->
            False


isReservedAtom : Term -> Bool
isReservedAtom t =
    case t of
        Atom (Num _) ->
            False

        Atom _ ->
            True

        _ ->
            False


isAtom : Term -> Bool
isAtom t =
    case t of
        Atom _ ->
            True

        _ ->
            False


occupyEntry : ( MetaVar, List Term ) -> ( MetaVar, List Term )
occupyEntry ( m, ts ) =
    case ts of
        [] ->
            ( m, [ Atom (MetaVar m) ] )

        _ :: _ ->
            ( m, ts )


occupy : List ( MetaVar, List Term ) -> List ( MetaVar, List Term )
occupy =
    List.map occupyEntry


isNotReal : List Term -> Term -> Bool
isNotReal context t =
    case t of
        Unary Neg t1 ->
            isNotReal context t1

        Unary Square t2 ->
            isNotReal context t2

        Binary Addition t3 t4 ->
            isNotReal context t3 || isNotReal context t4

        Binary Subtraction t5 t6 ->
            isNotReal context t5 || isNotReal context t6

        Binary Multiplication t7 t8 ->
            isNotReal context t7 || isNotReal context t8

        Binary Division t10 t9 ->
            isNotReal context t10
                || isNotReal context t9
                       || reducesToEqualTo t9 (Atom (Num Nat.zero)) || List.member (Binary EqualTo t9 (Atom (Num Nat.zero))) context

        _ ->
            False


rationalToTerm : Rat -> Term
rationalToTerm r =
    if Rat.isNegative r then
        if Rat.isInt r then
            Unary Neg (Atom (Num (Rat.toNumerator r)))
        else
            Unary Neg (Binary Division (Atom (Num (Rat.toNumerator r))) (Atom (Num (Rat.toDenominator r))))
    else if Rat.isInt r then
        Atom (Num (Rat.toNumerator r))
    else
        Binary Division (Atom (Num (Rat.toNumerator r))) (Atom (Num (Rat.toDenominator r)))


reduceArithmetic : Term -> Term
reduceArithmetic t =
    rationalToTerm (performArithmetic t)


isNotEqualToZero : List Term -> Term -> Bool
isNotEqualToZero context t =
    if List.member (Binary NotEqualTo t (Atom (Num Nat.zero))) context then
        True
    else
        case t of
            Atom (Num n1) ->
                not (n1 == Nat.zero)

            Atom _ ->
                False

            Unary Neg t1 ->
                isNotEqualToZero context t1

            Unary Square t2 ->
                isNotEqualToZero context t2

            Binary Addition _ _ ->
                False

            Binary Subtraction _ _ ->
                False

            Binary Multiplication t3 t4 ->
                isNotEqualToZero context t3 && isNotEqualToZero context t4

            Binary Division t5 t6 ->
                isNotEqualToZero context t5 && isNotEqualToZero context t6

            _ ->
                Debug.crash "impossible"


isReal : List Term -> Term -> Bool
isReal context t =
    case t of
        Atom AlgX ->
            True

        Atom AlgY ->
            True

        Atom AlgA ->
            True

        Atom AlgB ->
            True

        Atom AlgC ->
            True

        Atom AlgA1 ->
            True

        Atom AlgB1 ->
            True

        Atom AlgC1 ->
            True

        Atom AlgA2 ->
            True

        Atom AlgB2 ->
            True

        Atom AlgC2 ->
            True

        Atom (Num _) ->
            True

        Unary Neg t1 ->
            isReal context t1

        Unary Square t2 ->
            isReal context t2

        Binary Addition t3 t4 ->
            isReal context t3 && isReal context t4

        Binary Subtraction t5 t6 ->
            isReal context t5 && isReal context t6

        Binary Multiplication t7 t8 ->
            isReal context t7 && isReal context t8

        Binary Division t9 t10 ->
            isReal context t9 && isNotEqualToZero context t10

        _ ->
            False


reducesToFalse : List Term -> Term -> Bool
reducesToFalse context t =
    case t of
        Unary IsReal t1 ->
            isNotReal context t1

        Unary IsNotReal t2 ->
            isReal context t2

        Unary Not t3 ->
            reducesToTrue context t3

        Binary EqualTo t4 t5 ->
            reducesToNotEqualTo t4 t5

        Binary NotEqualTo t6 t7 ->
            reducesToEqualTo t6 t7

        _ ->
            False


reducesToTrue : List Term -> Term -> Bool
reducesToTrue context t =
    case t of
        Unary IsReal t1 ->
            isReal context t1

        Unary IsNotReal t2 ->
            isNotReal context t2

        Unary Not t3 ->
            reducesToFalse context t3

        Binary EqualTo t4 t5 ->
            reducesToEqualTo t4 t5

        Binary NotEqualTo t6 t7 ->
            reducesToNotEqualTo t6 t7

        _ ->
            False


startingDigits : List Term
startingDigits =
    [ Atom (Num (Nat.fromInt 0)), Atom (Num (Nat.fromInt 1)), Atom (Num (Nat.fromInt 2)) ]


variables : Term -> List Term
variables t =
    case t of
        Atom AlgX ->
            [ Atom AlgX ]

        Atom AlgY ->
            [ Atom AlgY ]

        Atom AlgA ->
            [ Atom AlgA ]

        Atom AlgB ->
            [ Atom AlgB ]

        Atom AlgC ->
            [ Atom AlgC ]

        Atom AlgA1 ->
            [ Atom AlgA1 ]

        Atom AlgB1 ->
            [ Atom AlgB1 ]

        Atom AlgC1 ->
            [ Atom AlgC1 ]

        Atom AlgA2 ->
            [ Atom AlgA2 ]

        Atom AlgB2 ->
            [ Atom AlgB2 ]

        Atom AlgC2 ->
            [ Atom AlgC2 ]

        Atom _ ->
            []

        Unary _ t1 ->
            variables t1

        Binary _ t2 t3 ->
            variables t2 ++ variables t3


decoder : Decoder Term
decoder =
    Json.Decode.oneOf
        [ Json.Decode.andThen
              (\s ->
                   case s of
                       "top" ->
                           Json.Decode.succeed (Atom Top)

                       "bot" ->
                           Json.Decode.succeed (Atom Bot)

                       "var-upper-a" ->
                           Json.Decode.succeed (Atom VarA)

                       "var-upper-b" ->
                           Json.Decode.succeed (Atom VarB)

                       "var-upper-c" ->
                           Json.Decode.succeed (Atom VarC)

                       "var-lower-a" ->
                           Json.Decode.succeed (Atom AlgA)

                       "var-lower-b" ->
                           Json.Decode.succeed (Atom AlgB)

                       "var-lower-c" ->
                           Json.Decode.succeed (Atom AlgC)

                       "var-lower-x" ->
                           Json.Decode.succeed (Atom AlgX)

                       "var-lower-y" ->
                           Json.Decode.succeed (Atom AlgY)

                       "var-lower-a-2" ->
                           Json.Decode.succeed (Atom AlgA2)

                       "var-lower-b-2" ->
                           Json.Decode.succeed (Atom AlgB2)

                       "var-lower-c-2" ->
                           Json.Decode.succeed (Atom AlgC2)

                       "var-lower-a-1" ->
                           Json.Decode.succeed (Atom AlgA1)

                       "var-lower-b-1" ->
                           Json.Decode.succeed (Atom AlgB1)

                       "var-lower-c-1" ->
                           Json.Decode.succeed (Atom AlgC1)

                       "meta-upper-a" ->
                           Json.Decode.succeed (Atom (MetaVar MetaA))

                       "meta-upper-b" ->
                           Json.Decode.succeed (Atom (MetaVar MetaB))

                       "meta-upper-c" ->
                           Json.Decode.succeed (Atom (MetaVar MetaC))

                       "meta-lower-a" ->
                           Json.Decode.succeed (Atom (MetaVar MetaAlgA))

                       "meta-lower-b" ->
                           Json.Decode.succeed (Atom (MetaVar MetaAlgB))

                       "meta-lower-c" ->
                           Json.Decode.succeed (Atom (MetaVar MetaAlgC))

                       "meta-lower-d" ->
                           Json.Decode.succeed (Atom (MetaVar MetaAlgD))

                       _ ->
                           Json.Decode.fail "term"
              )
              Json.Decode.string
        , Json.Decode.andThen
              (\s ->
                   case s of
                       "neg" ->
                           Json.Decode.map (Unary Neg) (Json.Decode.index 1 decoder)

                       "not" ->
                           Json.Decode.map (Unary Not) (Json.Decode.index 1 decoder)

                       "square" ->
                           Json.Decode.map (Unary Square) (Json.Decode.index 1 decoder)

                       "and" ->
                           Json.Decode.map2 (Binary Conjunction) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "or" ->
                           Json.Decode.map2 (Binary Disjunction) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "implies" ->
                           Json.Decode.map2 (Binary Implication) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "equivalent" ->
                           Json.Decode.map2 (Binary Equivalence) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "equal-to" ->
                           Json.Decode.map2 (Binary EqualTo) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "not-equal-to" ->
                           Json.Decode.map2 (Binary NotEqualTo) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "add" ->
                           Json.Decode.map2 (Binary Addition) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "subtract" ->
                           Json.Decode.map2 (Binary Subtraction) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "multiply" ->
                           Json.Decode.map2 (Binary Multiplication) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "divide" ->
                           Json.Decode.map2 (Binary Division) (Json.Decode.index 1 decoder) (Json.Decode.index 2 decoder)

                       "number" ->
                           Json.Decode.map (Atom << Num) (Json.Decode.index 1 Nat.decoder)

                       _ ->
                           Json.Decode.fail "term"
              )
              (Json.Decode.index 0 Json.Decode.string)
        ]
