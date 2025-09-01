module Wizard.Api.Models.QuestionnaireDetail.Comment exposing
    ( Comment
    , compare
    , decoder
    , isAuthor
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Time
import Time.Extra as Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)


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


compare : Comment -> Comment -> Order
compare a b =
    Time.compare a.createdAt b.createdAt
