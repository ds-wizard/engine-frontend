module Wizard.Common.Components.Questionnaire.Importer.ImporterEvent exposing
    ( AddItemData
    , ImporterEvent(..)
    , ReplyIntegrationData
    , ReplyItemSelectData
    , ReplyListData
    , ReplyStringData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type ImporterEvent
    = ReplyString ReplyStringData
    | ReplyList ReplyListData
    | ReplyIntegration ReplyIntegrationData
    | ReplyItemSelect ReplyItemSelectData
    | AddItem AddItemData


decoder : Decoder ImporterEvent
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder ImporterEvent
decoderByType eventType =
    case eventType of
        "ReplyString" ->
            D.map ReplyString decodeReplyStringData

        "ReplyList" ->
            D.map ReplyList decodeReplyListData

        "ReplyIntegration" ->
            D.map ReplyIntegration decodeReplyIntegrationData

        "ReplyItemSelect" ->
            D.map ReplyItemSelect decodeReplyItemSelectData

        "AddItem" ->
            D.map AddItem decodeAddItemData

        _ ->
            D.fail <| "Unknown ImporterEvent: " ++ eventType


type alias ReplyStringData =
    { path : String
    , value : String
    }


decodeReplyStringData : Decoder ReplyStringData
decodeReplyStringData =
    D.succeed ReplyStringData
        |> D.required "path" D.string
        |> D.required "value" D.string


type alias ReplyListData =
    { path : String
    , value : List String
    }


decodeReplyListData : Decoder ReplyListData
decodeReplyListData =
    D.succeed ReplyListData
        |> D.required "path" D.string
        |> D.required "value" (D.list D.string)


type alias ReplyIntegrationData =
    { path : String
    , value : String
    , id : Maybe String
    }


decodeReplyIntegrationData : Decoder ReplyIntegrationData
decodeReplyIntegrationData =
    D.succeed ReplyIntegrationData
        |> D.required "path" D.string
        |> D.required "value" D.string
        |> D.required "id" (D.maybe D.string)


type alias ReplyItemSelectData =
    { path : String
    , value : String
    }


decodeReplyItemSelectData : Decoder ReplyItemSelectData
decodeReplyItemSelectData =
    D.succeed ReplyItemSelectData
        |> D.required "path" D.string
        |> D.required "value" D.string


type alias AddItemData =
    { path : String
    , uuid : String
    }


decodeAddItemData : Decoder AddItemData
decodeAddItemData =
    D.succeed AddItemData
        |> D.required "path" D.string
        |> D.required "uuid" D.string
