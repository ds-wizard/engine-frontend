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
    , isSetLabels
    , isSetReplyList
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import List.Extra as List
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.ClearReplyData as ClearReplyData exposing (ClearReplyData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetLabelsData as SetLabelsData exposing (SetLabelsData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetLevelData as SetLevelData exposing (SetLevelData)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent.SetReplyData as SetReplyData exposing (SetReplyData)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Time
import Uuid exposing (Uuid)


type QuestionnaireEvent
    = SetReply SetReplyData
    | ClearReply ClearReplyData
    | SetLevel SetLevelData
    | SetLabels SetLabelsData


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

        "SetLevelEvent" ->
            D.map SetLevel SetLevelData.decoder

        "SetLabelsEvent" ->
            D.map SetLabels SetLabelsData.decoder

        _ ->
            D.fail <| "Unknown QuestionnaireEvent: " ++ actionType


encode : QuestionnaireEvent -> E.Value
encode =
    map
        SetReplyData.encode
        ClearReplyData.encode
        SetLevelData.encode
        SetLabelsData.encode


isSetLabels : QuestionnaireEvent -> Bool
isSetLabels event =
    case event of
        SetLabels _ ->
            True

        _ ->
            False


isSetReplyList : QuestionnaireEvent -> Bool
isSetReplyList event =
    case event of
        SetReply data ->
            case data.value of
                ItemListReply _ ->
                    True

                _ ->
                    False

        _ ->
            False


isInvisible : QuestionnaireEvent -> Bool
isInvisible event =
    case event of
        SetReply data ->
            case data.value of
                ItemListReply _ ->
                    True

                EmptyReply ->
                    True

                _ ->
                    False

        SetLabels _ ->
            True

        _ ->
            False


getUuid : QuestionnaireEvent -> Uuid
getUuid =
    map .uuid .uuid .uuid .uuid


getPath : QuestionnaireEvent -> Maybe String
getPath =
    map (.path >> Just) (.path >> Just) (always Nothing) (.path >> Just)


getCreatedAt : QuestionnaireEvent -> Time.Posix
getCreatedAt =
    map .createdAt .createdAt .createdAt .createdAt


getCreatedBy : QuestionnaireEvent -> Maybe UserSuggestion
getCreatedBy =
    map .createdBy .createdBy .createdBy .createdBy


getQuestionUuid : QuestionnaireEvent -> Maybe String
getQuestionUuid =
    Maybe.andThen (List.last << String.split ".") << getPath


map : (SetReplyData -> a) -> (ClearReplyData -> a) -> (SetLevelData -> a) -> (SetLabelsData -> a) -> QuestionnaireEvent -> a
map mapSetReply mapClearReply mapSetLevel mapSetLabels event =
    case event of
        SetReply data ->
            mapSetReply data

        ClearReply data ->
            mapClearReply data

        SetLevel data ->
            mapSetLevel data

        SetLabels data ->
            mapSetLabels data
