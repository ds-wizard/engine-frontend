module Wizard.Api.Models.KnowledgeModel.Question.CommonQuestionData exposing
    ( CommonQuestionData
    , decoder
    , encodeValues
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias CommonQuestionData =
    { uuid : String
    , title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , referenceUuids : List String
    , expertUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder CommonQuestionData
decoder =
    D.succeed CommonQuestionData
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "referenceUuids" (D.list D.string)
        |> D.required "expertUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encodeValues : CommonQuestionData -> List ( String, E.Value )
encodeValues commonData =
    [ ( "uuid", E.string commonData.uuid )
    , ( "title", E.string commonData.title )
    , ( "text", E.maybe E.string commonData.text )
    , ( "requiredPhaseUuid", E.maybe E.string commonData.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string commonData.tagUuids )
    , ( "referenceUuids", E.list E.string commonData.referenceUuids )
    , ( "expertUuids", E.list E.string commonData.expertUuids )
    , ( "annotations", E.list Annotation.encode commonData.annotations )
    ]
