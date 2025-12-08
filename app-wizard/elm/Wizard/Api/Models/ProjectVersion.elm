module Wizard.Api.Models.ProjectVersion exposing
    ( ProjectVersion
    , decoder
    , getVersionByEventUuid
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import List.Extra as List
import Time
import Uuid exposing (Uuid)


type alias ProjectVersion =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , eventUuid : Uuid
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


decoder : Decoder ProjectVersion
decoder =
    D.succeed ProjectVersion
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "eventUuid" Uuid.decoder
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)


getVersionByEventUuid : List ProjectVersion -> Uuid -> Maybe ProjectVersion
getVersionByEventUuid versions eventUuid =
    List.find (.eventUuid >> (==) eventUuid) versions
