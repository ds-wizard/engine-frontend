module Wizard.Api.Models.ApiKey exposing
    ( ApiKey
    , decoder
    , isActive
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias ApiKey =
    { uuid : Uuid
    , name : String
    , createdAt : Time.Posix
    , expiresAt : Time.Posix
    , userAgent : String
    , currentSession : Bool
    }


decoder : Decoder ApiKey
decoder =
    D.succeed ApiKey
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "createdAt" D.datetime
        |> D.required "expiresAt" D.datetime
        |> D.required "userAgent" D.string
        |> D.required "currentSession" D.bool


isActive : Time.Posix -> ApiKey -> Bool
isActive timestamp { expiresAt } =
    Time.posixToMillis expiresAt > Time.posixToMillis timestamp
