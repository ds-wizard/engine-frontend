module Wizard.Components.Questionnaire2.QuestionnaireVirtualizationTest exposing (crossReferencesTest)

import Expect
import Gettext
import Json.Decode as D
import Set
import Test exposing (Test, describe, test)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings as QuestionnaireViewSettings
import Wizard.Components.Questionnaire2.QuestionnaireVirtualization as QuestionnaireVirtualization exposing (ContentNode(..))
import Wizard.Routes


chapterUuid : String
chapterUuid =
    "11111111-1111-1111-1111-111111111111"


questionWithReferencesUuid : String
questionWithReferencesUuid =
    "22222222-2222-2222-2222-222222222222"


existingTargetUuid : String
existingTargetUuid =
    "33333333-3333-3333-3333-333333333333"


missingTargetUuid : String
missingTargetUuid =
    "99999999-9999-9999-9999-999999999999"


{-| Chapter with a question that has two cross references: one to a question that
exists in the (tag-filtered) knowledge model and one to a question that does not.
-}
knowledgeModelRaw : String
knowledgeModelRaw =
    """
    {
        "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
        "chapterUuids": ["11111111-1111-1111-1111-111111111111"],
        "tagUuids": [],
        "integrationUuids": [],
        "metricUuids": [],
        "phaseUuids": [],
        "resourceCollectionUuids": [],
        "annotations": [],
        "entities": {
            "chapters": {
                "11111111-1111-1111-1111-111111111111": {
                    "uuid": "11111111-1111-1111-1111-111111111111",
                    "title": "Chapter 1",
                    "text": null,
                    "questionUuids": ["22222222-2222-2222-2222-222222222222", "33333333-3333-3333-3333-333333333333"],
                    "annotations": []
                }
            },
            "questions": {
                "22222222-2222-2222-2222-222222222222": {
                    "uuid": "22222222-2222-2222-2222-222222222222",
                    "questionType": "ValueQuestion",
                    "valueType": "StringQuestionValueType",
                    "validations": [],
                    "title": "Question with references",
                    "text": null,
                    "requiredPhaseUuid": null,
                    "tagUuids": [],
                    "referenceUuids": ["aaaaaaaa-0000-0000-0000-000000000001", "aaaaaaaa-0000-0000-0000-000000000002"],
                    "expertUuids": [],
                    "annotations": []
                },
                "33333333-3333-3333-3333-333333333333": {
                    "uuid": "33333333-3333-3333-3333-333333333333",
                    "questionType": "ValueQuestion",
                    "valueType": "StringQuestionValueType",
                    "validations": [],
                    "title": "Existing target question",
                    "text": null,
                    "requiredPhaseUuid": null,
                    "tagUuids": [],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                }
            },
            "answers": {},
            "choices": {},
            "experts": {},
            "references": {
                "aaaaaaaa-0000-0000-0000-000000000001": {
                    "referenceType": "CrossReference",
                    "uuid": "aaaaaaaa-0000-0000-0000-000000000001",
                    "targetUuid": "33333333-3333-3333-3333-333333333333",
                    "description": "See also",
                    "annotations": []
                },
                "aaaaaaaa-0000-0000-0000-000000000002": {
                    "referenceType": "CrossReference",
                    "uuid": "aaaaaaaa-0000-0000-0000-000000000002",
                    "targetUuid": "99999999-9999-9999-9999-999999999999",
                    "description": "Filtered out",
                    "annotations": []
                }
            },
            "integrations": {},
            "tags": {},
            "metrics": {},
            "phases": {},
            "resourceCollections": {},
            "resourcePages": {}
        }
    }
    """


knowledgeModel : KnowledgeModel
knowledgeModel =
    Result.withDefault KnowledgeModel.empty (D.decodeString KnowledgeModel.decoder knowledgeModelRaw)


crossReferencesOf : String -> List QuestionnaireVirtualization.QuestionExtraCrossReference
crossReferencesOf targetQuestionUuid =
    let
        questionnaire =
            ProjectQuestionnaire.createQuestionnaireDetail KnowledgeModelPackage.dummy knowledgeModel

        ctx =
            { chapterUuid = chapterUuid
            , questionnaire = questionnaire
            , collapsedPaths = Set.empty
            , resourcePageToUrl = always Wizard.Routes.dashboard
            , viewSettings = QuestionnaireViewSettings.all
            , locale = Gettext.defaultLocale
            }
    in
    QuestionnaireVirtualization.virtualizeChapter ctx
        |> List.filterMap
            (\node ->
                case node of
                    QuestionNode data ->
                        if Question.getUuid data.question == targetQuestionUuid then
                            Just data.questionExtraData.crossReferences

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.concat


crossReferencesTest : Test
crossReferencesTest =
    describe "QuestionnaireVirtualization.virtualizeChapter cross references"
        [ test "keeps cross references whose target question exists" <|
            \_ ->
                crossReferencesOf questionWithReferencesUuid
                    |> List.map .targetQuestionUuid
                    |> Expect.equal [ existingTargetUuid ]
        , test "drops cross references whose target question was filtered out" <|
            \_ ->
                crossReferencesOf questionWithReferencesUuid
                    |> List.any (\ref -> ref.targetQuestionUuid == missingTargetUuid)
                    |> Expect.equal False
        ]
