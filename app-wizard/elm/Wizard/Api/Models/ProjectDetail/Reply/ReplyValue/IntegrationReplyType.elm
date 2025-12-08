module Wizard.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType exposing
    ( IntegrationReplyType(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Json.Value as JsonValue exposing (JsonValue)


type IntegrationReplyType
    = PlainType String
    | IntegrationType String JsonValue
    | IntegrationLegacyType (Maybe String) String


decoder : Decoder IntegrationReplyType
decoder =
    D.oneOf
        [ D.when integrationReplyType ((==) "PlainType") decodePlainType
        , D.when integrationReplyType ((==) "IntegrationType") decodeIntegrationType
        , D.when integrationReplyType ((==) "IntegrationLegacyType") decodeIntegrationLegacyType
        ]


integrationReplyType : Decoder String
integrationReplyType =
    D.field "type" D.string


decodePlainType : Decoder IntegrationReplyType
decodePlainType =
    D.succeed PlainType
        |> D.required "value" D.string


decodeIntegrationType : Decoder IntegrationReplyType
decodeIntegrationType =
    D.succeed IntegrationType
        |> D.required "value" D.string
        |> D.required "raw" JsonValue.decoder


decodeIntegrationLegacyType : Decoder IntegrationReplyType
decodeIntegrationLegacyType =
    D.succeed IntegrationLegacyType
        |> D.required "id" (D.maybe D.string)
        |> D.required "value" D.string


encode : IntegrationReplyType -> E.Value
encode replyType =
    case replyType of
        PlainType value ->
            E.object
                [ ( "type", E.string "IntegrationReply" )
                , ( "value"
                  , E.object
                        [ ( "type", E.string "PlainType" )
                        , ( "value", E.string value )
                        ]
                  )
                ]

        IntegrationType value raw ->
            E.object
                [ ( "type", E.string "IntegrationReply" )
                , ( "value"
                  , E.object
                        [ ( "type", E.string "IntegrationType" )
                        , ( "value", E.string value )
                        , ( "raw", JsonValue.encode raw )
                        ]
                  )
                ]

        IntegrationLegacyType id value ->
            E.object
                [ ( "type", E.string "IntegrationReply" )
                , ( "value"
                  , E.object
                        [ ( "type", E.string "IntegrationLegacyType" )
                        , ( "id", E.maybe E.string id )
                        , ( "value", E.string value )
                        ]
                  )
                ]
