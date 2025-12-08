module Wizard.Api.Models.ProjectFile exposing
    ( ProjectFile
    , decoder
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectInfo as ProjectInfo exposing (ProjectInfo)


type alias ProjectFile =
    { uuid : Uuid
    , contentType : String
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    , fileName : String
    , fileSize : Int
    , project : ProjectInfo
    }


decoder : Decoder ProjectFile
decoder =
    D.succeed ProjectFile
        |> D.required "uuid" Uuid.decoder
        |> D.required "contentType" D.string
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "fileName" D.string
        |> D.required "fileSize" D.int
        |> D.required "project" ProjectInfo.decoder
