module Shared.Data.Plan exposing (Plan, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time


type alias Plan =
    { name : String
    , until : Time.Posix
    , since : Time.Posix
    , test : Bool
    , users : Maybe Int
    }


decoder : Decoder Plan
decoder =
    D.succeed Plan
        |> D.required "name" D.string
        |> D.required "until" D.datetime
        |> D.required "since" D.datetime
        |> D.required "test" D.bool
        |> D.required "users" (D.maybe D.int)
