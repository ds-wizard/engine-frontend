module Shared.Data.QuestionnaireDetail.CommentThread exposing (CommentThread, decoder, isAuthor)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.Data.QuestionnaireDetail.Comment as Comment exposing (Comment)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Uuid exposing (Uuid)


type alias CommentThread =
    { uuid : Uuid
    , resolved : Bool
    , comments : List Comment
    , private : Bool
    , createdBy : Maybe UserSuggestion
    }


decoder : Decoder CommentThread
decoder =
    D.succeed CommentThread
        |> D.required "uuid" Uuid.decoder
        |> D.required "resolved" D.bool
        |> D.required "comments" (D.list Comment.decoder)
        |> D.required "private" D.bool
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)


isAuthor : Maybe { u | uuid : Uuid } -> CommentThread -> Bool
isAuthor user commentThread =
    let
        toUserUuid =
            Maybe.map .uuid
    in
    Maybe.isJust user && toUserUuid commentThread.createdBy == toUserUuid user
