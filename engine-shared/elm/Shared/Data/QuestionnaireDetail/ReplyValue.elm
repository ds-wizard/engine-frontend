module Shared.Data.QuestionnaireDetail.ReplyValue exposing
    ( ReplyValue(..)
    , decoder
    , encode
    , getAnswerUuid
    , getItemListCount
    , getItemUuids
    , getStringReply
    , isEmpty
    , isEmptyReply
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.QuestionnaireDetail.ReplyValue.IntegrationReplyValue as IntegrationReplyValue exposing (IntegrationReplyValue(..))


type ReplyValue
    = StringReply String
    | AnswerReply String
    | ItemListReply (List String)
    | EmptyReply
    | IntegrationReply IntegrationReplyValue


decoder : Decoder ReplyValue
decoder =
    D.oneOf
        [ D.when replyValueType ((==) "StringReply") decodeStringReply
        , D.when replyValueType ((==) "AnswerReply") decodeAnswerReply
        , D.when replyValueType ((==) "ItemListReply") decodeItemListReply
        , D.when replyValueType ((==) "IntegrationReply") decodeIntegrationReply
        ]


replyValueType : Decoder String
replyValueType =
    D.field "type" D.string


decodeStringReply : Decoder ReplyValue
decodeStringReply =
    D.succeed StringReply
        |> D.required "value" D.string


decodeAnswerReply : Decoder ReplyValue
decodeAnswerReply =
    D.succeed AnswerReply
        |> D.required "value" D.string


decodeItemListReply : Decoder ReplyValue
decodeItemListReply =
    D.succeed ItemListReply
        |> D.required "value" (D.list D.string)


decodeIntegrationReply : Decoder ReplyValue
decodeIntegrationReply =
    D.succeed IntegrationReply
        |> D.required "value" IntegrationReplyValue.decoder


encode : ReplyValue -> E.Value
encode replyValue =
    case replyValue of
        StringReply string ->
            E.object
                [ ( "type", E.string "StringReply" )
                , ( "value", E.string string )
                ]

        AnswerReply uuid ->
            E.object
                [ ( "type", E.string "AnswerReply" )
                , ( "value", E.string uuid )
                ]

        ItemListReply itemUuids ->
            E.object
                [ ( "type", E.string "ItemListReply" )
                , ( "value", E.list E.string itemUuids )
                ]

        EmptyReply ->
            E.null

        IntegrationReply integrationReplyValue ->
            IntegrationReplyValue.encode integrationReplyValue


getItemListCount : ReplyValue -> Int
getItemListCount replyValue =
    case replyValue of
        ItemListReply itemUuids ->
            List.length itemUuids

        _ ->
            0


getItemUuids : ReplyValue -> List String
getItemUuids replyValue =
    case replyValue of
        ItemListReply itemUuids ->
            itemUuids

        _ ->
            []


getAnswerUuid : ReplyValue -> String
getAnswerUuid replyValue =
    case replyValue of
        AnswerReply uuid ->
            uuid

        _ ->
            ""


getStringReply : ReplyValue -> String
getStringReply replyValue =
    case replyValue of
        StringReply string ->
            string

        IntegrationReply integrationReplyValue ->
            case integrationReplyValue of
                PlainValue value ->
                    value

                IntegrationValue id value ->
                    value

        _ ->
            ""


isEmptyReply : ReplyValue -> Bool
isEmptyReply replyValue =
    case replyValue of
        EmptyReply ->
            True

        _ ->
            False


isEmpty : ReplyValue -> Bool
isEmpty replyValue =
    case replyValue of
        EmptyReply ->
            True

        StringReply str ->
            String.isEmpty str

        ItemListReply items ->
            List.isEmpty items

        _ ->
            False
