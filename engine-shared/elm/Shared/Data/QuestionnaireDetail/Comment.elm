module Shared.Data.QuestionnaireDetail.Comment exposing
    ( Comment
    , decoder
    , isAuthor
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type alias Comment =
    { uuid : Uuid
    , text : String
    , createdBy : Maybe UserSuggestion
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


decoder : Decoder Comment
decoder =
    D.succeed Comment
        |> D.required "uuid" Uuid.decoder
        |> D.required "text" D.string
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime


isAuthor : Maybe { u | uuid : Uuid } -> Comment -> Bool
isAuthor user comment =
    let
        toUserUuid =
            Maybe.map .uuid
    in
    Maybe.isJust user && toUserUuid comment.createdBy == toUserUuid user
