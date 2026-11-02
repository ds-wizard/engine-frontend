module Common.Components.Questionnaire.Fixture exposing
    ( chapter1
    , chapter2
    , followUp
    , list
    , options
    , optionsAnswer
    , questionnaire
    , reply
    , template
    , value1
    )

{-| A small questionnaire with one of each shape the node list has to get right:
options whose first answer opens a follow-up, a list whose items have a question
of their own, and a value question in each of two chapters.
-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue)
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Dict exposing (Dict)
import Json.Decode as D
import Time


chapter1 : String
chapter1 =
    "c1111111-1111-4111-8111-111111111111"


chapter2 : String
chapter2 =
    "c2222222-2222-4222-8222-222222222222"


options : String
options =
    "a1111111-1111-4111-8111-111111111111"


optionsAnswer : String
optionsAnswer =
    "a2222222-2222-4222-8222-222222222222"


followUp : String
followUp =
    "a3333333-3333-4333-8333-333333333333"


list : String
list =
    "b1111111-1111-4111-8111-111111111111"


template : String
template =
    "b2222222-2222-4222-8222-222222222222"


value1 : String
value1 =
    "d1111111-1111-4111-8111-111111111111"


value2 : String
value2 =
    "d2222222-2222-4222-8222-222222222222"


reply : ReplyValue -> Reply
reply value =
    { value = value, createdAt = Time.millisToPosix 0, createdBy = Nothing }


questionnaire : Dict String Reply -> ProjectQuestionnaire
questionnaire replies =
    let
        base =
            ProjectQuestionnaire.createQuestionnaireDetail KnowledgeModelPackage.dummy knowledgeModel
    in
    { base | replies = replies }


knowledgeModel : KnowledgeModel
knowledgeModel =
    Result.withDefault KnowledgeModel.empty (D.decodeString KnowledgeModel.decoder raw)


valueQuestion : String -> String -> String
valueQuestion uuid title =
    """
    "UUID": {
        "uuid": "UUID", "questionType": "ValueQuestion", "valueType": "StringQuestionValueType", "validations": [],
        "title": "TITLE", "text": null, "requiredPhaseUuid": null, "tagUuids": [], "referenceUuids": [], "expertUuids": [], "annotations": []
    }
    """
        |> String.replace "UUID" uuid
        |> String.replace "TITLE" title


raw : String
raw =
    """
    {
        "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
        "chapterUuids": ["CHAPTER1", "CHAPTER2"],
        "tagUuids": [], "integrationUuids": [], "metricUuids": [], "phaseUuids": [], "resourceCollectionUuids": [], "annotations": [],
        "entities": {
            "chapters": {
                "CHAPTER1": { "uuid": "CHAPTER1", "title": "Chapter 1", "text": null, "questionUuids": ["OPTIONS", "LIST", "VALUE1"], "annotations": [] },
                "CHAPTER2": { "uuid": "CHAPTER2", "title": "Chapter 2", "text": null, "questionUuids": ["VALUE2"], "annotations": [] }
            },
            "questions": {
                "OPTIONS": {
                    "uuid": "OPTIONS", "questionType": "OptionsQuestion", "answerUuids": ["ANSWER", "e1111111-1111-4111-8111-111111111111"],
                    "title": "Options", "text": null, "requiredPhaseUuid": null, "tagUuids": [], "referenceUuids": [], "expertUuids": [], "annotations": []
                },
                "LIST": {
                    "uuid": "LIST", "questionType": "ListQuestion", "itemTemplateQuestionUuids": ["TEMPLATE"],
                    "title": "List", "text": null, "requiredPhaseUuid": null, "tagUuids": [], "referenceUuids": [], "expertUuids": [], "annotations": []
                },
                FOLLOWUP_Q,
                TEMPLATE_Q,
                VALUE1_Q,
                VALUE2_Q
            },
            "answers": {
                "ANSWER": { "uuid": "ANSWER", "label": "Yes", "advice": null, "metricMeasures": [], "followUpUuids": ["FOLLOWUP"], "annotations": [] },
                "e1111111-1111-4111-8111-111111111111": { "uuid": "e1111111-1111-4111-8111-111111111111", "label": "No", "advice": null, "metricMeasures": [], "followUpUuids": [], "annotations": [] }
            },
            "choices": {}, "experts": {}, "references": {}, "resourceCollections": {}, "resourcePages": {}, "integrations": {}, "tags": {}, "metrics": {}, "phases": {}
        }
    }
    """
        |> String.replace "FOLLOWUP_Q" (valueQuestion followUp "Follow-up")
        |> String.replace "TEMPLATE_Q" (valueQuestion template "Item name")
        |> String.replace "VALUE1_Q" (valueQuestion value1 "Value 1")
        |> String.replace "VALUE2_Q" (valueQuestion value2 "Value 2")
        |> String.replace "CHAPTER1" chapter1
        |> String.replace "CHAPTER2" chapter2
        |> String.replace "OPTIONS" options
        |> String.replace "ANSWER" optionsAnswer
        |> String.replace "FOLLOWUP" followUp
        |> String.replace "LIST" list
        |> String.replace "TEMPLATE" template
        |> String.replace "VALUE1" value1
        |> String.replace "VALUE2" value2
