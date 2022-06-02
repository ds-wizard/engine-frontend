module Shared.Data.QuestionnaireDetail.Reply.ReplyValue exposing
    ( ReplyValue(..)
    , decoder
    , encode
    , getAnswerUuid
    , getChoiceUuid
    , getItemUuids
    , getStringReply
    , isEmpty
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyValue exposing (IntegrationReplyType(..))


type ReplyValue
    = StringReply String
    | AnswerReply String
    | MultiChoiceReply (List String)
    | ItemListReply (List String)
    | IntegrationReply IntegrationReplyType


decoder : Decoder ReplyValue
decoder =
    D.oneOf
        [ D.when replyValueType ((==) "StringReply") decodeStringReply
        , D.when replyValueType ((==) "AnswerReply") decodeAnswerReply
        , D.when replyValueType ((==) "MultiChoiceReply") decodeMultiChoiceReply
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


decodeMultiChoiceReply : Decoder ReplyValue
decodeMultiChoiceReply =
    D.succeed MultiChoiceReply
        |> D.required "value" (D.list D.string)


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

        MultiChoiceReply choiceUuids ->
            E.object
                [ ( "type", E.string "MultiChoiceReply" )
                , ( "value", E.list E.string choiceUuids )
                ]

        ItemListReply itemUuids ->
            E.object
                [ ( "type", E.string "ItemListReply" )
                , ( "value", E.list E.string itemUuids )
                ]

        IntegrationReply integrationReplyValue ->
            IntegrationReplyValue.encode integrationReplyValue


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


getChoiceUuid : ReplyValue -> List String
getChoiceUuid replyValue =
    case replyValue of
        MultiChoiceReply uuids ->
            uuids

        _ ->
            []


getStringReply : ReplyValue -> String
getStringReply replyValue =
    case replyValue of
        StringReply string ->
            string

        IntegrationReply integrationReplyValue ->
            case integrationReplyValue of
                PlainType value ->
                    value

                IntegrationType _ value ->
                    value

        _ ->
            ""


isEmpty : ReplyValue -> Bool
isEmpty replyValue =
    case replyValue of
        StringReply str ->
            String.isEmpty str

        ItemListReply items ->
            List.isEmpty items

        MultiChoiceReply choices ->
            List.isEmpty choices

        _ ->
            False
