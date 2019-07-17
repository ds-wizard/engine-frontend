module Questionnaires.Common.QuestionnaireDetail exposing
    ( QuestionnaireDetail
    , decoder
    , encode
    , setLevel
    , updateLabels
    , updateReplies
    )

import FormEngine.Model exposing (FormValues, decodeFormValues, encodeFormValues)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, knowledgeModelDecoder)
import KnowledgeModels.Common.Package as Package exposing (Package)
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
import Questionnaires.Common.QuestionnaireLabel as QuestionnaireLabel exposing (QuestionnaireLabel)


type alias QuestionnaireDetail =
    { uuid : String
    , name : String
    , package : Package
    , knowledgeModel : KnowledgeModel
    , replies : FormValues
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , ownerUuid : Maybe String
    , selectedTagUuids : List String
    , labels : List QuestionnaireLabel
    }


decoder : Decoder QuestionnaireDetail
decoder =
    D.succeed QuestionnaireDetail
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "package" Package.decoder
        |> D.required "knowledgeModel" knowledgeModelDecoder
        |> D.required "replies" decodeFormValues
        |> D.required "level" D.int
        |> D.required "accessibility" QuestionnaireAccessibility.decoder
        |> D.required "ownerUuid" (D.maybe D.string)
        |> D.required "selectedTagUuids" (D.list D.string)
        |> D.required "labels" (D.list QuestionnaireLabel.decoder)


encode : QuestionnaireDetail -> E.Value
encode questionnaire =
    E.object
        [ ( "name", E.string questionnaire.name )
        , ( "accessibility", QuestionnaireAccessibility.encode questionnaire.accessibility )
        , ( "replies", encodeFormValues questionnaire.replies )
        , ( "level", E.int questionnaire.level )
        , ( "labels", E.list QuestionnaireLabel.encode questionnaire.labels )
        ]


updateReplies : FormValues -> QuestionnaireDetail -> QuestionnaireDetail
updateReplies replies questionnaire =
    { questionnaire | replies = replies }


updateLabels : List QuestionnaireLabel -> QuestionnaireDetail -> QuestionnaireDetail
updateLabels labels questionnaire =
    { questionnaire | labels = labels }


setLevel : QuestionnaireDetail -> Int -> QuestionnaireDetail
setLevel questionnaire level =
    { questionnaire | level = level }
