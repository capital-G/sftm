module Step exposing (..)

import Path exposing (Path)
import Term exposing (Term)
import Json.Encode exposing (Value)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline


type alias Step =
    { rule : Int
    , isReversed : Bool
    , term : Term
    , path : Path
    }


encoder : Step -> Value
encoder x =
    Json.Encode.object
        [ ( "rule", Json.Encode.int x.rule )
        , ( "is-reversed", Json.Encode.bool x.isReversed )
        , ( "term", Term.encoder x.term )
        , ( "path", Path.encoder x.path )
        ]


decoder : Decoder Step
decoder =
    Json.Decode.Pipeline.required
        "path"
        Path.decoder
        (Json.Decode.Pipeline.required
             "term"
             Term.decoder
             (Json.Decode.Pipeline.required
                  "is-reversed"
                  Json.Decode.bool
                  (Json.Decode.Pipeline.required "rule" Json.Decode.int (Json.Decode.Pipeline.decode Step))
             )
        )
