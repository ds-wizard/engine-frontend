module Shared.Data.QuestionnaireDetail.ReplyValue.IntegrationReplyValue exposing
    ( IntegrationReplyValue(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E


type IntegrationReplyValue
    = PlainValue String
    | IntegrationValue String String


decoder : Decoder IntegrationReplyValue
decoder =
    D.oneOf
        [ D.when integrationValueType ((==) "PlainValue") decodePlainValue
        , D.when integrationValueType ((==) "IntegrationValue") decodeIntegrationValue
        ]


integrationValueType : Decoder String
integrationValueType =
    D.field "type" D.string


decodePlainValue : Decoder IntegrationReplyValue
decodePlainValue =
    D.succeed PlainValue
        |> D.required "value" D.string


decodeIntegrationValue : Decoder IntegrationReplyValue
decodeIntegrationValue =
    D.succeed IntegrationValue
        |> D.required "id" D.string
        |> D.required "value" D.string


encode : IntegrationReplyValue -> E.Value
encode integrationReplyValue =
    case integrationReplyValue of
        PlainValue value ->
            E.object
                [ ( "type", E.string "IntegrationReply" )
                , ( "value"
                  , E.object
                        [ ( "type", E.string "PlainValue" )
                        , ( "value", E.string value )
                        ]
                  )
                ]

        IntegrationValue id value ->
            E.object
                [ ( "type", E.string "IntegrationReply" )
                , ( "value"
                  , E.object
                        [ ( "type", E.string "IntegrationValue" )
                        , ( "id", E.string id )
                        , ( "value", E.string value )
                        ]
                  )
                ]
