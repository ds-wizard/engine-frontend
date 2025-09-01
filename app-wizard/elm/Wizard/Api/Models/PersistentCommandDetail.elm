module Wizard.Api.Models.PersistentCommandDetail exposing
    ( PersistentCommandDetail
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.TenantSuggestion as TenantSuggestion exposing (TenantSuggestion)
import Wizard.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)


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
    , tenant : TenantSuggestion
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
        |> D.required "tenant" TenantSuggestion.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime
