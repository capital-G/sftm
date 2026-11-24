module Problem exposing (..)

import Html exposing (Html)
import Rule exposing (Rule)
import Step exposing (Step)
import Term exposing (Term)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline
import Json.Encode exposing (Value)
import Path exposing (Path)


{-| Problem descriptions use this record.
-}
type alias StaticProblem =
    { description : String
    , notes : List (Html Never)
    , start : Term
    , finish : Term
    , rules : List Rule
    , scratch : List Term
    }


{-| A problem as stored in the problem list.
-}
type alias Problem =
    { description : String
    , notes : List (Html Never)
    , rules : List Rule
    , scratch : List Term
    , scratchSymbols : List Term
    , solved : Bool
    , start : Term
    , finish : Term
    , history : List Step
    , future : List Step
    , proof : Maybe (List Step)
    }


{-| This information is saved to local storage.
-}
type alias LocalStorageProblem =
    { scratch : List Term
    , solved : Bool
    , history : List Step
    , future : List Step
    , proof : Maybe (List Step)
    }


reset : Problem -> Problem
reset problem =
    { problem
        | scratch = Term.uniqueSortVariables (Term.variables problem.start ++ Term.variables problem.finish) ++ Term.startingDigits
        , history = []
        , future = []
        }


restoreProblem : LocalStorageProblem -> Problem -> Problem
restoreProblem x p =
    { p
        | scratch = x.scratch
        , solved = x.solved
        , history = x.history
        , future = x.future
        , proof = x.proof
    }


saveProblem : Problem -> LocalStorageProblem
saveProblem x =
    { scratch = x.scratch
    , solved = x.solved
    , history = x.history
    , future = x.future
    , proof = x.proof
    }


localStorageEncoder : LocalStorageProblem -> Value
localStorageEncoder x =
    Json.Encode.object
        [ ( "scratch", Json.Encode.list (List.map Term.encoder x.scratch) )
        , ( "solved", Json.Encode.bool x.solved )
        , ( "history", Json.Encode.list (List.map Step.encoder x.history) )
        , ( "future", Json.Encode.list (List.map Step.encoder x.future) )
        , ( "proof", Maybe.withDefault Json.Encode.null (Maybe.map (Json.Encode.list << List.map Step.encoder) x.proof) )
        ]


encoder : Problem -> Value
encoder problem =
    localStorageEncoder (saveProblem problem)


createScratch : { a | finish : Term, start : Term } -> List Term
createScratch p =
    Term.uniqueSortVariables (Term.variables p.start ++ Term.variables p.finish) ++ Term.startingDigits


initProblem : { f | description : b , notes : c , rules : d , scratch : e , start : Term , finish : Term } -> { description : b , finish : Term , future : List g , history : List h , notes : c , proof : Maybe a , rules : d , scratch : List Term , scratchSymbols : e , solved : Bool , start : Term }
initProblem problemStatic =
    { description = problemStatic.description
    , notes = problemStatic.notes
    , rules = problemStatic.rules
    , scratchSymbols = problemStatic.scratch
    , scratch = createScratch problemStatic
    , solved = False
    , start = problemStatic.start
    , finish = problemStatic.finish
    , history = []
    , future = []
    , proof = Nothing
    }


init : List StaticProblem -> List Problem
init =
    List.map initProblem


localStorageDecoder : Decoder LocalStorageProblem
localStorageDecoder =
    Json.Decode.Pipeline.decode LocalStorageProblem
        |> Json.Decode.Pipeline.required "scratch" (Json.Decode.list Term.decoder)
        |> Json.Decode.Pipeline.required "solved" Json.Decode.bool
        |> Json.Decode.Pipeline.required "history" (Json.Decode.list Step.decoder)
        |> Json.Decode.Pipeline.required "future" (Json.Decode.list Step.decoder)
        |> Json.Decode.Pipeline.required "proof" (Json.Decode.nullable (Json.Decode.list Step.decoder))
