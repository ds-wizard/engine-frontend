module Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing
    ( QuestionnaireEvent(..)
    , decoder
    , encode
    , getCreatedAt
    , getCreatedBy
    , getPath
    , getQuestionUuid
    , getUuid
    , isInvisible
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra as List
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.AddCommentData as AddCommentData exposing (AddCommentData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.ClearReplyData as ClearReplyData exposing (ClearReplyData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.DeleteCommentData as DeleteCommentData exposing (DeleteCommentData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.DeleteCommentThreadData as DeleteCommentThreadData exposing (DeleteCommentThreadData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.EditCommentData as EditCommentData exposing (EditCommentData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.ReopenCommentThreadData as ReopenCommentThreadData exposing (ReopenCommentThreadData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.ResolveCommentThreadData as ResolveCommentThreadData exposing (ResolveCommentThreadData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetLabelsData as SetLabelsData exposing (SetLabelsData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetPhaseData as SetPhaseData exposing (SetPhaseData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData as SetReplyData exposing (SetReplyData)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type QuestionnaireEvent
    = SetReply SetReplyData
    | ClearReply ClearReplyData
    | SetPhase SetPhaseData
    | SetLabels SetLabelsData
    | ResolveCommentThread ResolveCommentThreadData
    | ReopenCommentThread ReopenCommentThreadData
    | DeleteCommentThread DeleteCommentThreadData
    | AddComment AddCommentData
    | EditComment EditCommentData
    | DeleteComment DeleteCommentData


decoder : Decoder QuestionnaireEvent
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder QuestionnaireEvent
decoderByType actionType =
    case actionType of
        "SetReplyEvent" ->
            D.map SetReply SetReplyData.decoder

        "ClearReplyEvent" ->
            D.map ClearReply ClearReplyData.decoder

        "SetPhaseEvent" ->
            D.map SetPhase SetPhaseData.decoder

        "SetLabelsEvent" ->
            D.map SetLabels SetLabelsData.decoder

        "ResolveCommentThreadEvent" ->
            D.map ResolveCommentThread ResolveCommentThreadData.decoder

        "ReopenCommentThreadEvent" ->
            D.map ReopenCommentThread ReopenCommentThreadData.decoder

        "DeleteCommentThreadEvent" ->
            D.map DeleteCommentThread DeleteCommentThreadData.decoder

        "AddCommentEvent" ->
            D.map AddComment AddCommentData.decoder

        "EditCommentEvent" ->
            D.map EditComment EditCommentData.decoder

        "DeleteCommentEvent" ->
            D.map DeleteComment DeleteCommentData.decoder

        _ ->
            D.fail <| "Unknown QuestionnaireEvent: " ++ actionType


encode : QuestionnaireEvent -> E.Value
encode =
    map
        SetReplyData.encode
        ClearReplyData.encode
        SetPhaseData.encode
        SetLabelsData.encode
        ResolveCommentThreadData.encode
        ReopenCommentThreadData.encode
        DeleteCommentThreadData.encode
        AddCommentData.encode
        EditCommentData.encode
        DeleteCommentData.encode


isInvisible : QuestionnaireEvent -> Bool
isInvisible event =
    case event of
        SetReply data ->
            case data.value of
                ItemListReply _ ->
                    True

                _ ->
                    False

        SetPhase _ ->
            False

        ClearReply _ ->
            False

        _ ->
            True


getUuid : QuestionnaireEvent -> Uuid
getUuid =
    map .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid


getPath : QuestionnaireEvent -> Maybe String
getPath =
    map (.path >> Just) (.path >> Just) (always Nothing) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just)


getCreatedAt : QuestionnaireEvent -> Time.Posix
getCreatedAt =
    map .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt


getCreatedBy : QuestionnaireEvent -> Maybe UserSuggestion
getCreatedBy =
    map .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy


getQuestionUuid : QuestionnaireEvent -> Maybe String
getQuestionUuid =
    Maybe.andThen (List.last << String.split ".") << getPath


map :
    (SetReplyData -> a)
    -> (ClearReplyData -> a)
    -> (SetPhaseData -> a)
    -> (SetLabelsData -> a)
    -> (ResolveCommentThreadData -> a)
    -> (ReopenCommentThreadData -> a)
    -> (DeleteCommentThreadData -> a)
    -> (AddCommentData -> a)
    -> (EditCommentData -> a)
    -> (DeleteCommentData -> a)
    -> QuestionnaireEvent
    -> a
map mapSetReply mapClearReply mapSetLevel mapSetLabels mapResolveCommentThread mapReopenCommentThread mapDeleteCommentThread mapAddComment mapEditComment mapDeleteComment event =
    case event of
        SetReply data ->
            mapSetReply data

        ClearReply data ->
            mapClearReply data

        SetPhase data ->
            mapSetLevel data

        SetLabels data ->
            mapSetLabels data

        ResolveCommentThread data ->
            mapResolveCommentThread data

        ReopenCommentThread data ->
            mapReopenCommentThread data

        DeleteCommentThread data ->
            mapDeleteCommentThread data

        AddComment data ->
            mapAddComment data

        EditComment data ->
            mapEditComment data

        DeleteComment data ->
            mapDeleteComment data
