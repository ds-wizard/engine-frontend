module Shared.Data.PersistentCommand exposing
    ( PersistentCommand
    , decoder
    , visibleName
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Shared.Data.TenantSuggestion as TenantSuggestion exposing (TenantSuggestion)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias PersistentCommand =
    { uuid : Uuid
    , component : String
    , function : String
    , state : PersistentCommandState
    , attempts : Int
    , maxAttempts : Int
    , createdBy : Maybe UserSuggestion
    , tenant : TenantSuggestion
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Decoder PersistentCommand
decoder =
    D.succeed PersistentCommand
        |> D.required "uuid" Uuid.decoder
        |> D.required "component" D.string
        |> D.required "function" D.string
        |> D.required "state" PersistentCommandState.decoder
        |> D.required "attempts" D.int
        |> D.required "maxAttempts" D.int
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "tenant" TenantSuggestion.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime


visibleName : { a | component : String, function : String } -> String
visibleName persistentCommand =
    persistentCommand.component ++ " :: " ++ persistentCommand.function
