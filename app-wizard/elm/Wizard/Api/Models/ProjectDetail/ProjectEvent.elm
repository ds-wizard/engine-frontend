module Wizard.Api.Models.ProjectDetail.ProjectEvent exposing
    ( ProjectEvent(..)
    , decoder
    , encode
    , getCreatedAt
    , getCreatedBy
    , getPath
    , getQuestionUuid
    , getUuid
    , isInvisible
    )

import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra as List
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.AddCommentData as AddCommentData exposing (AddCommentData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.AssignCommentThreadData as AssignCommentThreadData exposing (AssignCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.ClearReplyData as ClearReplyData exposing (ClearReplyData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.DeleteCommentData as DeleteCommentData exposing (DeleteCommentData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.DeleteCommentThreadData as DeleteCommentThreadData exposing (DeleteCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.EditCommentData as EditCommentData exposing (EditCommentData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.ReopenCommentThreadData as ReopenCommentThreadData exposing (ReopenCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.ResolveCommentThreadData as ResolveCommentThreadData exposing (ResolveCommentThreadData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetLabelsData as SetLabelsData exposing (SetLabelsData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetPhaseData as SetPhaseData exposing (SetPhaseData)
import Wizard.Api.Models.ProjectDetail.ProjectEvent.SetReplyData as SetReplyData exposing (SetReplyData)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))


type ProjectEvent
    = SetReply SetReplyData
    | ClearReply ClearReplyData
    | SetPhase SetPhaseData
    | SetLabels SetLabelsData
    | ResolveCommentThread ResolveCommentThreadData
    | ReopenCommentThread ReopenCommentThreadData
    | DeleteCommentThread DeleteCommentThreadData
    | AssignCommentThread AssignCommentThreadData
    | AddComment AddCommentData
    | EditComment EditCommentData
    | DeleteComment DeleteCommentData


decoder : Decoder ProjectEvent
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ProjectEvent
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

        "AssignCommentThreadEvent" ->
            D.map AssignCommentThread AssignCommentThreadData.decoder

        "AddCommentEvent" ->
            D.map AddComment AddCommentData.decoder

        "EditCommentEvent" ->
            D.map EditComment EditCommentData.decoder

        "DeleteCommentEvent" ->
            D.map DeleteComment DeleteCommentData.decoder

        _ ->
            D.fail <| "Unknown QuestionnaireEvent: " ++ actionType


encode : ProjectEvent -> E.Value
encode =
    map
        SetReplyData.encode
        ClearReplyData.encode
        SetPhaseData.encode
        SetLabelsData.encode
        ResolveCommentThreadData.encode
        ReopenCommentThreadData.encode
        DeleteCommentThreadData.encode
        AssignCommentThreadData.encode
        AddCommentData.encode
        EditCommentData.encode
        DeleteCommentData.encode


isInvisible : ProjectEvent -> Bool
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


getUuid : ProjectEvent -> Uuid
getUuid =
    map .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid .uuid


getPath : ProjectEvent -> Maybe String
getPath =
    map (.path >> Just) (.path >> Just) (always Nothing) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just) (.path >> Just)


getCreatedAt : ProjectEvent -> Time.Posix
getCreatedAt =
    map .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt .createdAt


getCreatedBy : ProjectEvent -> Maybe UserSuggestion
getCreatedBy =
    map .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy .createdBy


getQuestionUuid : ProjectEvent -> Maybe String
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
    -> (AssignCommentThreadData -> a)
    -> (AddCommentData -> a)
    -> (EditCommentData -> a)
    -> (DeleteCommentData -> a)
    -> ProjectEvent
    -> a
map mapSetReply mapClearReply mapSetLevel mapSetLabels mapResolveCommentThread mapReopenCommentThread mapDeleteCommentThread mapAssignCommentThread mapAddComment mapEditComment mapDeleteComment event =
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

        AssignCommentThread data ->
            mapAssignCommentThread data

        AddComment data ->
            mapAddComment data

        EditComment data ->
            mapEditComment data

        DeleteComment data ->
            mapDeleteComment data
