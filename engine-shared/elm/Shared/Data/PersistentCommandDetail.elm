module Shared.Data.PersistentCommandDetail exposing
    ( PersistentCommandDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.AppSuggestion as AppSuggestion exposing (AppSuggestion)
import Shared.Data.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias PersistentCommandDetail =
    { uuid : Uuid
    , component : String
    , function : String
    , state : PersistentCommandState
    , body : String
    , lastErrorMessage : Maybe String
    , lastTraceUuid : Maybe Uuid
    , attempts : Int
    , maxAttempts : Int
    , createdBy : Maybe UserSuggestion
    , app : AppSuggestion
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Decoder PersistentCommandDetail
decoder =
    D.succeed PersistentCommandDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "component" D.string
        |> D.required "function" D.string
        |> D.required "state" PersistentCommandState.decoder
        |> D.required "body" D.string
        |> D.required "lastErrorMessage" (D.maybe D.string)
        |> D.required "lastTraceUuid" (D.maybe Uuid.decoder)
        |> D.required "attempts" D.int
        |> D.required "maxAttempts" D.int
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "app" AppSuggestion.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime
