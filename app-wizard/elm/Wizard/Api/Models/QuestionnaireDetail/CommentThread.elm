module Wizard.Api.Models.QuestionnaireDetail.CommentThread exposing (CommentThread, commentCount, compare, decoder, isAssigned, isAuthor)

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Time
import Time.Extra as Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireDetail.Comment as Comment exposing (Comment)


type alias CommentThread =
    { uuid : Uuid
    , resolved : Bool
    , comments : List Comment
    , private : Bool
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    , assignedTo : Maybe UserSuggestion
    }


decoder : Decoder CommentThread
decoder =
    D.succeed CommentThread
        |> D.required "uuid" Uuid.decoder
        |> D.required "resolved" D.bool
        |> D.required "comments" (D.list Comment.decoder)
        |> D.required "private" D.bool
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "assignedTo" (D.maybe UserSuggestion.decoder)


isAuthor : Maybe { u | uuid : Uuid } -> CommentThread -> Bool
isAuthor user commentThread =
    let
        toUserUuid =
            Maybe.map .uuid
    in
    Maybe.isJust user && toUserUuid commentThread.createdBy == toUserUuid user


compare : CommentThread -> CommentThread -> Order
compare a b =
    Time.compare a.createdAt b.createdAt


commentCount : CommentThread -> Int
commentCount =
    List.length << .comments


isAssigned : CommentThread -> Bool
isAssigned =
    Maybe.isJust << .assignedTo
