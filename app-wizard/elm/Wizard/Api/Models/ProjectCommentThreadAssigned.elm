module Wizard.Api.Models.ProjectCommentThreadAssigned exposing
    ( ProjectCommentThreadAssigned
    , decoder
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


type alias ProjectCommentThreadAssigned =
    { commentThreadUuid : Uuid
    , createdBy : Maybe UserSuggestion
    , path : String
    , private : Bool
    , projectName : String
    , projectUuid : Uuid
    , resolved : Bool
    , text : String
    , updatedAt : Time.Posix
    }


decoder : Decoder ProjectCommentThreadAssigned
decoder =
    D.succeed ProjectCommentThreadAssigned
        |> D.required "commentThreadUuid" Uuid.decoder
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "path" D.string
        |> D.required "private" D.bool
        |> D.required "projectName" D.string
        |> D.required "projectUuid" Uuid.decoder
        |> D.required "resolved" D.bool
        |> D.required "text" D.string
        |> D.required "updatedAt" D.datetime
