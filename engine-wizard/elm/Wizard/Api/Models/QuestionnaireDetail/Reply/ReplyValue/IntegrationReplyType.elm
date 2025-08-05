module Wizard.Api.Models.QuestionnaireDetail.Reply.ReplyValue.IntegrationReplyType exposing
    ( IntegrationReplyType(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type IntegrationReplyType
    = PlainType String
    | IntegrationType (Maybe String) String


decoder : Decoder IntegrationReplyType
decoder =
    D.oneOf
        [ D.when integrationReplyType ((==) "PlainType") decodePlainType
        , D.when integrationReplyType ((==) "IntegrationType") decodeIntegrationType
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

        IntegrationType id value ->
            E.object
                [ ( "type", E.string "IntegrationReply" )
                , ( "value"
                  , E.object
                        [ ( "type", E.string "IntegrationType" )
                        , ( "id", E.maybe E.string id )
                        , ( "value", E.string value )
                        ]
                  )
                ]
