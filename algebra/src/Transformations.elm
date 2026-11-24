module Transformations exposing (..)

import Rule
import Term exposing (Term, MetaVar)
import Transformation exposing (Transformation)
import Nat


type Direction
    = Leftwards
    | Rightwards


type alias Dictionary =
    List ( MetaVar, List Term )


type alias Rule =
    { name : String
    , isSymmetric : Bool
    , antecedents : List Term
    , left : Term
    , right : Term
    , index : Int
    }



-- Let's try creating a dictionary of metavariables. Then we unify, failing if
-- two different terms have the same metavariable. Then we add the scratch for
-- any empty metavariables. Then we attempt to satisfy the antecedents for any
-- combination entries. We need to return Single or Multiple, but we don't need
-- to return the matches.


clearDictionary : Dictionary -> Dictionary
clearDictionary =
    List.map (\( mvar, _ ) -> ( mvar, [] ))


checkAntecedent : List Term -> Term -> Bool
checkAntecedent context antecedent =
    Term.reducesToTrue context antecedent || List.any (\t -> t == antecedent) context


lookup : MetaVar -> List ( MetaVar, List Term ) -> Term
lookup m1 dict =
    case dict of
        [] ->
            Debug.crash "impossible"

        ( _, [] ) :: _ ->
            Debug.crash "impossible"

        ( a1, [ x1 ] ) :: rest ->
            if m1 == a1 then
                x1
            else
                lookup m1 rest

        ( a2, (_ :: (_ :: _)) ) :: rest1 ->
            if m1 == a2 then
                Term.Atom (Term.MetaVar m1)
            else
                lookup m1 rest1


dictionaryAddScratch : List Term -> Dictionary -> Dictionary
dictionaryAddScratch scratch =
    let
        f ( v, xs ) =
            if List.isEmpty xs then
                ( v, scratch )
            else
                ( v, xs )
    in
        List.map f


dictionaryHasEmpty : Dictionary -> Bool
dictionaryHasEmpty =
    List.any (\( v, xs ) -> List.isEmpty xs)


substitute : Dictionary -> Term -> Term
substitute dict term =
    case term of
        Term.Atom (Term.MetaVar m) ->
            lookup m dict

        Term.Atom a ->
            Term.Atom a

        Term.Unary u t1 ->
            Term.Unary u (substitute dict t1)

        Term.Binary b t1 t2 ->
            Term.Binary b (substitute dict t1) (substitute dict t2)


isSingleton : List a -> Bool
isSingleton xs =
    case xs of
        [ _ ] ->
            True

        _ ->
            False


uniq : List Term -> List Term -> List Term
uniq ts1 ts2 =
    case ( ts1, ts2 ) of
        ( [], ts2 ) ->
            ts2

        ( [ t1 ], [] ) ->
            [ t1 ]

        ( [ t1 ], t2 :: ts2 ) ->
            if t1 == t2 then
                t2 :: ts2
            else
                t2 :: uniq [ t1 ] ts2

        ( t1 :: ts1, ts2 ) ->
            uniq ts1 (uniq [ t1 ] ts2)


dictionaryMerge : Dictionary -> Dictionary -> Dictionary
dictionaryMerge d1 d2 =
    let
        f ( v, ts1 ) ( _, ts2 ) =
            ( v, uniq ts1 ts2 )
    in
        List.map2 f d1 d2


restrict : Rule -> b -> List Term -> Dictionary -> Dictionary
restrict rule sub context dict =
    let
        recur attempt dict result =
            case dict of
                [] ->
                    if List.all (\ant -> checkAntecedent context (substitute attempt ant)) rule.antecedents then
                        dictionaryMerge attempt result
                    else
                        result

                ( a1, b1 ) :: rest ->
                    List.foldl (\term -> recur (( a1, [ term ] ) :: attempt) rest) result b1
    in
    recur [] dict (clearDictionary dict)


everyScratch2 : (Dictionary -> Bool) -> List Term -> Dictionary -> Dictionary -> Dictionary
everyScratch2 p scratch dict =
    let
        f dict1 dict2 result =
            case dict1 of
                [] ->
                    if p dict2 then
                        dictionaryMerge result dict2
                    else
                        result

                ( v, [] ) :: dict1r ->
                    List.foldr (\t -> f dict1r (( v, [ t ] ) :: dict2)) result scratch

                ( v, xs ) :: dict1r ->
                    f dict1r (( v, xs ) :: dict2) result
    in
        f (List.reverse dict) []


dictionaryAddUnique : MetaVar -> Term -> Dictionary -> Maybe Dictionary
dictionaryAddUnique v1 t1 dict =
    case dict of
        [] ->
            Debug.crash "impossible"

        ( v2, [] ) :: dict2 ->
            if v1 == v2 then
                Just (( v2, [ t1 ] ) :: dict2)
            else
                dictionaryAddUnique v1 t1 dict2 |> Maybe.map (\d -> ( v2, [] ) :: d)

        ( v2, [ t2 ] ) :: dict2 ->
            if v1 == v2 then
                if t1 == t2 then
                    Just (( v2, [ t2 ] ) :: dict2)
                else
                    Nothing
            else
                dictionaryAddUnique v1 t1 dict2 |> Maybe.map (\d -> ( v2, [ t2 ] ) :: d)

        ( v2, _ ) :: dict2 ->
            Debug.crash "impossible"


target : { a | rule : Rule, direction : Direction } -> Term
target x =
    case x.direction of
        Leftwards ->
            x.rule.left

        Rightwards ->
            x.rule.right


origin : { a | rule : Rule, direction : Direction } -> Term
origin x =
    case x.direction of
        Leftwards ->
            x.rule.right

        Rightwards ->
            x.rule.left


isPossible : { rule : Rule, direction : Direction, dictionary : Dictionary } -> Bool
isPossible x =
    List.all (\( v, xs ) -> not (List.isEmpty xs)) x.dictionary


addScratch : List Term -> { rule : Rule, direction : Direction, dictionary : Dictionary } -> { rule : Rule, direction : Direction, dictionary : Dictionary }
addScratch scratch x =
    let
        f ( v, xs ) =
            if List.isEmpty xs then
                ( v, scratch )
            else
                ( v, xs )
    in
        { x | dictionary = List.map f x.dictionary }


checkDictionary : Term -> List Term -> Term -> Dictionary -> Bool
checkDictionary focus scratch target dict =
    if dictionaryHasEmpty dict then
        case scratch of
            [] ->
                False

            [ _ ] ->
                focus /= substitute (dictionaryAddScratch scratch dict) target

            _ ->
                True
    else
        focus /= substitute dict target


everyScratch : (Dictionary -> Bool) -> List Term -> List ( MetaVar, List Term ) -> Maybe Dictionary -> Maybe Dictionary
everyScratch p scratch dict =
    let
        f dict1 dict2 result =
            case dict1 of
                [] ->
                    if p dict2 then
                        case result of
                            Nothing ->
                                Just dict2

                            Just x2 ->
                                Just (dictionaryMerge x2 dict2)
                    else
                        result

                ( a1, [] ) :: rest ->
                    let
                        scratch2 =
                            if a1 == Term.MetaAlgA || a1 == Term.MetaAlgB || a1 == Term.MetaAlgC || a1 == Term.MetaAlgD then
                                List.filter Term.isAlgebraic scratch1
                            else
                                List.filter Term.isLogical scratch1
                    in
                    List.foldl (\t -> f rest (( a1, [ t ] ) :: dict2)) result scratch2

                ( a2, ((_ :: _) as b2) ) :: rest1 ->
                    f rest1 (( a2, b2 ) :: dict2) result

        scratch1 =
            List.foldl g (List.reverse scratch) dict

        g ( _, b1 ) sc =
            case b1 of
                [] ->
                    sc

                [ x1 ] ->
                    if Term.isArithmetic x1 then
                        let
                            n =
                                Term.reduceArithmetic x1
                        in
                        if List.member n scratch then
                            sc
                        else
                            n :: sc
                    else
                        sc

                _ :: (_ :: _) ->
                    sc
    in
    f (List.reverse dict) []


unify : Term -> Term -> Dictionary -> Maybe Dictionary
unify focus term dict =
    case ( focus, term ) of
        ( (Term.Atom (Term.MetaVar _)), _ ) ->
            Just dict

        ( a1, (Term.Atom (Term.MetaVar Term.MetaA)) ) ->
            if Term.isLogical a1 then
                dictionaryAddUnique Term.MetaA a1 dict
            else
                Nothing

        ( a2, (Term.Atom (Term.MetaVar Term.MetaB)) ) ->
            if Term.isLogical a2 then
                dictionaryAddUnique Term.MetaB a2 dict
            else
                Nothing

        ( a3, (Term.Atom (Term.MetaVar Term.MetaC)) ) ->
            if Term.isLogical a3 then
                dictionaryAddUnique Term.MetaC a3 dict
            else
                Nothing

        ( a4, (Term.Atom (Term.MetaVar Term.MetaAlgA)) ) ->
            if Term.isAlgebraic a4 then
                dictionaryAddUnique Term.MetaAlgA a4 dict
            else
                Nothing

        ( a5, (Term.Atom (Term.MetaVar Term.MetaAlgB)) ) ->
            if Term.isAlgebraic a5 then
                dictionaryAddUnique Term.MetaAlgB a5 dict
            else
                Nothing

        ( a6, (Term.Atom (Term.MetaVar Term.MetaAlgC)) ) ->
            if Term.isAlgebraic a6 then
                dictionaryAddUnique Term.MetaAlgC a6 dict
            else
                Nothing

        ( a7, (Term.Atom (Term.MetaVar Term.MetaAlgD)) ) ->
            if Term.isAlgebraic a7 then
                dictionaryAddUnique Term.MetaAlgD a7 dict
            else
                Nothing

        ( (Term.Atom a8), (Term.Atom a9) ) ->
            if a8 == a9 then
                Just dict
            else
                Nothing

        ( (Term.Unary op t1), (Term.Unary op1 t2) ) ->
            if op == op1 then
                unify t1 t2 dict
            else
                Nothing

        ( (Term.Binary op2 t5 t3), (Term.Binary op3 t6 t4) ) ->
            if op2 == op3 then
                Maybe.andThen (unify t3 t4) (unify t5 t6 dict)
            else
                Nothing

        ( _, _ ) ->
            Nothing


antecedents : Term -> List Term -> List Term -> { a | direction : Direction , rule : Rule , dictionary : List ( MetaVar, List Term ) } -> Maybe { a | dictionary : List ( MetaVar, List Term ) , direction : Direction , rule : Rule }
antecedents focus scratch context x =
    if x.rule.name == "Arithmetic" then
        case x.dictionary of
            [ ( _, [ x1 ] ) ] ->
                let
                    ts =
                        case x.direction of
                            Rightwards ->
                                case x1 of
                                    Term.Binary Term.EqualTo t4 t3 ->
                                        if Term.reducesToEqualTo t4 t3 then
                                            [ Term.Atom Term.Top ]
                                        else if Term.reducesToNotEqualTo t4 t3 then
                                            [ Term.Atom Term.Bot ]
                                        else
                                            []

                                    Term.Binary Term.NotEqualTo t6 t5 ->
                                        if Term.reducesToEqualTo t6 t5 then
                                            [ Term.Atom Term.Bot ]
                                        else if Term.reducesToEqualTo t6 t5 then
                                            [ Term.Atom Term.Top ]
                                        else
                                            []

                                    _ ->
                                        if Term.isArithmetic x1 then
                                            let
                                                t =
                                                    Term.reduceArithmetic x1
                                            in
                                            if x1 == t then
                                                []
                                            else
                                                [ t ]
                                        else
                                            []

                            Leftwards ->
                                case x1 of
                                    Term.Atom Term.Top ->
                                        List.filter (Term.reducesToTrue []) scratch

                                    Term.Atom Term.Bot ->
                                        List.filter (Term.reducesToFalse []) scratch

                                    _ ->
                                        if Term.isArithmetic x1 then
                                            let
                                                t =
                                                    Term.reduceArithmetic x1
                                            in
                                            if x1 == t then
                                                List.filter (\t -> not (x1 == t) && Term.isArithmetic t && x1 == Term.reduceArithmetic t) scratch
                                            else
                                                []
                                        else
                                            []
                in
                Just { x | dictionary = [ ( Term.MetaAlgA, ts ) ] }

            _ ->
                Debug.crash "impossible"
    else
        let
            result =
                List.foldl
                    (f (\dict -> not (focus == substitute dict (target x))) x.rule.antecedents x.dictionary)
                    Nothing
                    (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)

            f p antecedents dict term result =
                case antecedents of
                    [] ->
                        everyScratch p scratch dict result

                    x2 :: rest ->
                        case term of
                            Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) ->
                                case x2 of
                                    Term.Unary Term.IsReal t7 ->
                                        List.foldl
                                            (f (\dict -> checkIsReal dict t7 && p dict) rest dict)
                                            result
                                            (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)

                                    Term.Unary Term.IsNotReal t8 ->
                                        List.foldl
                                            (f (\dict -> checkIsNotReal dict t8 && p dict) rest dict)
                                            result
                                            (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)

                                    Term.Binary Term.EqualTo t10 t9 ->
                                        if t9 == Term.Atom (Term.Num Nat.zero) then
                                            List.foldl
                                                (f (\dict -> checkIsEqual dict t10 t9 && p dict) rest dict)
                                                result
                                                (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)
                                        else
                                            result

                                    Term.Binary Term.NotEqualTo t12 t11 ->
                                        if t11 == Term.Atom (Term.Num Nat.zero) then
                                            List.foldl
                                                (f (\dict -> checkIsNotEqualToZero dict t12 && p dict) rest dict)
                                                result
                                                (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)
                                        else
                                            result

                                    _ ->
                                        result

                            Term.Binary Term.EqualTo t14 t13 ->
                                case unify term x2 dict of
                                    Nothing ->
                                        case unify (Term.Binary Term.EqualTo t13 t14) x2 dict of
                                            Nothing ->
                                                result

                                            Just x3 ->
                                                List.foldl (f p rest x3) result (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)

                                    Just x4 ->
                                        List.foldl (f p rest x4) result (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)

                            _ ->
                                case unify term x2 dict of
                                    Nothing ->
                                        result

                                    Just x5 ->
                                        List.foldl (f p rest x5) result (Term.Unary Term.IsReal (Term.Atom (Term.MetaVar Term.MetaAlgA)) :: context)

            checkIsReal dict t =
                Term.isReal context (substitute dict t)

            checkIsNotReal dict t =
                Term.isNotReal context (substitute dict t)

            checkIsEqual dict t1 t2 =
                Term.reducesToEqualTo (substitute dict t1) (substitute dict t2)

            checkIsNotEqual dict t1 t2 =
                Term.reducesToNotEqualTo (substitute dict t1) (substitute dict t2)

            checkIsNotEqualToZero dict t =
                Term.isNotEqualToZero [] (substitute dict t)
        in
        Maybe.map (\d -> { x | dictionary = d }) result


reallyAntecedents focus right scratch context antecedents dict =
    let
        f antecedents dict term result =
            case antecedents of
                [] ->
                    everyScratch2 (\dict -> focus /= substitute dict right) scratch dict result

                ant :: ants ->
                    case unify term ant dict of
                        Nothing ->
                            result

                        Just newDict ->
                            List.foldr (f ants newDict) result context
    in
        List.foldr (f antecedents dict) dict context


performUnification : Term -> { a | direction : Direction, rule : Rule, dictionary : Dictionary } -> Maybe { a | dictionary : List ( MetaVar, List Term ) , direction : Direction , rule : Rule }
performUnification focus x =
    if x.rule.name == "Arithmetic" then
        Just { x | dictionary = [ ( Term.MetaA, [ focus ] ) ] }
    else
        Maybe.map (\d -> { x | dictionary = d }) (unify focus (origin x) x.dictionary)


ruleDictionary : Rule -> List ( MetaVar, List Term )
ruleDictionary rule =
    List.foldl Term.addMetavariables [] (rule.left :: rule.right :: rule.antecedents)


addDictionary : { c | direction : b , rule : Rule } -> { dictionary : List ( MetaVar, List Term ) , direction : b , rule : Rule }
addDictionary x =
    { rule = x.rule, direction = x.direction, dictionary = ruleDictionary x.rule }


fromRule : Rule -> Rule.Rule
fromRule rule =
    { name = rule.name
    , isSymmetric = rule.isSymmetric
    , antecedents = rule.antecedents
    , left = rule.left
    , right = rule.right
    }


toRule : Int -> Rule.Rule -> Rule
toRule idx rule =
    { name = rule.name
    , isSymmetric = rule.isSymmetric
    , antecedents = rule.antecedents
    , left = rule.left
    , right = rule.right
    , index = idx
    }


addDirection : Rule -> List { rule : Rule, direction : Direction } -> List { rule : Rule, direction : Direction }
addDirection rule xs =
    if rule.isSymmetric then
        { rule = rule, direction = Rightwards } :: xs
    else
        { rule = rule, direction = Rightwards } :: { rule = rule, direction = Leftwards } :: xs


populate : Term -> List Term -> List Term -> Rule -> Bool -> Dictionary -> List ( MetaVar, List Term )
populate focus scratch context rule isReversed dict =
    case
        performUnification
            focus
            { rule = rule
            , direction =
                  if isReversed then
                      Rightwards
                  else
                      Leftwards
            , dictionary = dict
            }
    of
        Nothing ->
            Debug.crash "impossible2"

        Just x1 ->
            case antecedents focus scratch context x1 of
                Just x2 ->
                    x2.dictionary

                Nothing ->
                    Debug.crash "impossible3"


toTransformation : { a | dictionary : List ( MetaVar, List Term ) , direction : Direction , rule : Rule } -> { display : Term, isMultiple : Bool, isReversed : Bool, ruleIndex : Int }
toTransformation x =
    if x.rule.name == "Arithmetic" then
        { isMultiple = List.any (\( _, b1 ) -> not (isSingleton b1)) x.dictionary
        , display =
              case x.dictionary of
                  [ ( _, [ x1 ] ) ] ->
                      x1

                  [ ( _, (x2 :: (_ :: _)) ) ] ->
                      if Term.isAlgebraic x2 then
                          Term.Atom (Term.MetaVar Term.MetaAlgA)
                      else
                          Term.Atom (Term.MetaVar Term.MetaA)

                  _ ->
                      Debug.crash "impossible"
        , isReversed = x.direction == Leftwards
        , ruleIndex = x.rule.index
        }
    else
        { isMultiple = List.any (\( _, b2 ) -> not (isSingleton b2)) x.dictionary
        , display = substitute x.dictionary (target x)
        , isReversed = x.direction == Leftwards
        , ruleIndex = x.rule.index
        }


transformations : List Rule.Rule -> List Term -> List Term -> Term -> List { display : Term , isMultiple : Bool , isReversed : Bool , ruleIndex : Int }
transformations rules scratch context focus =
    List.map
        toTransformation
        (List.filter
             isPossible
             (List.filterMap
                  (antecedents focus scratch context)
                  (List.filterMap
                       (performUnification focus)
                       (List.map addDictionary (List.foldr addDirection [] (List.indexedMap toRule rules)))
                  )
             )
        )
