port module Ports exposing (..)




{-| All ports.
-}
{-| Swaps the welcome screen for the Elm view.
-}
port initialize : () -> Cmd msg


{-| Google Analytics event.
-}
port sendEvent : { category : String, action : String, label : String, value : Int } -> Cmd msg


{-| Google Analytics event.
-}
port setPage : String -> Cmd msg


port setLocalStorage : String -> Cmd msg


port onLocalStorageChange : (Maybe String -> msg) -> Sub msg


{-| Copy to clipboard.
-}
port copy : String -> Cmd msg


{-| Opens the feedback email.
-}
port email : () -> Cmd msg


problemSolved : { b | problemIndex : number, remaining : a, stepCount : Int } -> Cmd msg
problemSolved x =
    sendEvent
        { category = "Problem-" ++ String.padLeft 2 '0' (toString (x.problemIndex + 1))
        , action = "Solved"
        , label = "Remaining-" ++ String.padLeft 2 '0' (toString x.remaining)
        , value = max (min x.stepCount 99) 1
        }
