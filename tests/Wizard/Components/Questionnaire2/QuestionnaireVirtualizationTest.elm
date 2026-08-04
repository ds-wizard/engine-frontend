module Wizard.Components.Questionnaire2.QuestionnaireVirtualizationTest exposing (crossReferencesTest, itemEmptyNodeTest)

import Dict exposing (Dict)
import Expect
import Gettext
import Json.Decode as D
import Set
import Test exposing (Test, describe, test)
import Time
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Wizard.Api.Models.ProjectDetail.Reply exposing (Reply)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Components.Questionnaire2.QuestionnaireVirtualization as QuestionnaireVirtualization exposing (ContentNode(..))
import Wizard.Routes


chapterUuid : String
chapterUuid =
    "11111111-1111-1111-1111-111111111111"



-- Shared helpers


decodeKnowledgeModel : String -> KnowledgeModel
decodeKnowledgeModel raw =
    Result.withDefault KnowledgeModel.empty (D.decodeString KnowledgeModel.decoder raw)


virtualize : KnowledgeModel -> Dict String Reply -> QuestionnaireViewSettings -> List ContentNode
virtualize knowledgeModel replies viewSettings =
    let
        base =
            ProjectQuestionnaire.createQuestionnaireDetail KnowledgeModelPackage.dummy knowledgeModel

        ctx =
            { chapterUuid = chapterUuid
            , questionnaire = { base | replies = replies }
            , collapsedPaths = Set.empty
            , resourcePageToUrl = always Wizard.Routes.dashboard
            , viewSettings = viewSettings
            , locale = Gettext.defaultLocale
            , knowledgeModelParentMap = KnowledgeModel.createParentMap knowledgeModel
            }
    in
    QuestionnaireVirtualization.virtualizeChapter ctx


reply : ReplyValue -> Reply
reply value =
    { value = value, createdAt = Time.millisToPosix 0, createdBy = Nothing }



-- Cross references


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
crossReferencesKmRaw : String
crossReferencesKmRaw =
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


crossReferencesOf : String -> List QuestionnaireVirtualization.QuestionExtraCrossReference
crossReferencesOf targetQuestionUuid =
    virtualize (decodeKnowledgeModel crossReferencesKmRaw) Dict.empty QuestionnaireViewSettings.all
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
        , test "prefixes the cross reference title with the target question's chapter number" <|
            \_ ->
                crossReferencesOf questionWithReferencesUuid
                    |> List.map .targetQuestionTitle
                    |> Expect.equal [ "Ch. 1: Existing target question" ]
        ]



-- Empty items


listWithVisibleUuid : String
listWithVisibleUuid =
    "22222222-2222-2222-2222-222222222222"


listAllHiddenUuid : String
listAllHiddenUuid =
    "44444444-4444-4444-4444-444444444444"


listNoQuestionsUuid : String
listNoQuestionsUuid =
    "66666666-6666-6666-6666-666666666666"


{-| Chapter with three list questions:

  - one item template question that is always visible,
  - one item template question that is non-desirable (hidden when the current
    phase is earlier than its required phase and non-desirable questions are off),
  - no item template questions at all.

-}
emptyItemsKmRaw : String
emptyItemsKmRaw =
    """
    {
        "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
        "chapterUuids": ["11111111-1111-1111-1111-111111111111"],
        "tagUuids": [],
        "integrationUuids": [],
        "metricUuids": [],
        "phaseUuids": ["0948bd26-d985-4549-b7c8-95e9061d6413", "64217c4e-50b3-4230-9224-bf65c4220ab6"],
        "resourceCollectionUuids": [],
        "annotations": [],
        "entities": {
            "chapters": {
                "11111111-1111-1111-1111-111111111111": {
                    "uuid": "11111111-1111-1111-1111-111111111111",
                    "title": "Chapter 1",
                    "text": null,
                    "questionUuids": ["22222222-2222-2222-2222-222222222222", "44444444-4444-4444-4444-444444444444", "66666666-6666-6666-6666-666666666666"],
                    "annotations": []
                }
            },
            "questions": {
                "22222222-2222-2222-2222-222222222222": {
                    "uuid": "22222222-2222-2222-2222-222222222222",
                    "questionType": "ListQuestion",
                    "title": "List with a visible question",
                    "text": null,
                    "requiredPhaseUuid": "0948bd26-d985-4549-b7c8-95e9061d6413",
                    "tagUuids": [],
                    "itemTemplateQuestionUuids": ["33333333-3333-3333-3333-333333333333"],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                },
                "33333333-3333-3333-3333-333333333333": {
                    "uuid": "33333333-3333-3333-3333-333333333333",
                    "questionType": "ValueQuestion",
                    "valueType": "StringQuestionValueType",
                    "validations": [],
                    "title": "Visible item question",
                    "text": null,
                    "requiredPhaseUuid": "0948bd26-d985-4549-b7c8-95e9061d6413",
                    "tagUuids": [],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                },
                "44444444-4444-4444-4444-444444444444": {
                    "uuid": "44444444-4444-4444-4444-444444444444",
                    "questionType": "ListQuestion",
                    "title": "List with only a hidden question",
                    "text": null,
                    "requiredPhaseUuid": "0948bd26-d985-4549-b7c8-95e9061d6413",
                    "tagUuids": [],
                    "itemTemplateQuestionUuids": ["55555555-5555-5555-5555-555555555555"],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                },
                "55555555-5555-5555-5555-555555555555": {
                    "uuid": "55555555-5555-5555-5555-555555555555",
                    "questionType": "ValueQuestion",
                    "valueType": "StringQuestionValueType",
                    "validations": [],
                    "title": "Non-desirable item question",
                    "text": null,
                    "requiredPhaseUuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                    "tagUuids": [],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                },
                "66666666-6666-6666-6666-666666666666": {
                    "uuid": "66666666-6666-6666-6666-666666666666",
                    "questionType": "ListQuestion",
                    "title": "List with no questions",
                    "text": null,
                    "requiredPhaseUuid": "0948bd26-d985-4549-b7c8-95e9061d6413",
                    "tagUuids": [],
                    "itemTemplateQuestionUuids": [],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                }
            },
            "answers": {},
            "choices": {},
            "experts": {},
            "references": {},
            "integrations": {},
            "tags": {},
            "metrics": {},
            "phases": {
                "0948bd26-d985-4549-b7c8-95e9061d6413": {
                    "uuid": "0948bd26-d985-4549-b7c8-95e9061d6413",
                    "title": "Phase 1",
                    "description": null,
                    "annotations": []
                },
                "64217c4e-50b3-4230-9224-bf65c4220ab6": {
                    "uuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                    "title": "Phase 2",
                    "description": null,
                    "annotations": []
                }
            },
            "resourceCollections": {},
            "resourcePages": {}
        }
    }
    """


emptyItemsReplies : Dict String Reply
emptyItemsReplies =
    Dict.fromList
        [ ( chapterUuid ++ "." ++ listWithVisibleUuid, reply (ItemListReply [ "item-visible" ]) )
        , ( chapterUuid ++ "." ++ listAllHiddenUuid, reply (ItemListReply [ "item-hidden" ]) )
        , ( chapterUuid ++ "." ++ listNoQuestionsUuid, reply (ItemListReply [ "item-none" ]) )
        ]


emptyItemNodes : QuestionnaireViewSettings -> List QuestionnaireVirtualization.ItemEmptyNodeData
emptyItemNodes viewSettings =
    virtualize (decodeKnowledgeModel emptyItemsKmRaw) emptyItemsReplies viewSettings
        |> List.filterMap
            (\node ->
                case node of
                    ItemEmptyNode data ->
                        Just data

                    _ ->
                        Nothing
            )


emptyNodeFor : String -> QuestionnaireViewSettings -> Maybe QuestionnaireVirtualization.ItemEmptyNodeData
emptyNodeFor listQuestionUuid viewSettings =
    let
        prefix =
            chapterUuid ++ "." ++ listQuestionUuid ++ "."
    in
    emptyItemNodes viewSettings
        |> List.filter (\data -> String.startsWith prefix data.itemPath)
        |> List.head


itemEmptyNodeTest : Test
itemEmptyNodeTest =
    describe "QuestionnaireVirtualization.virtualizeChapter empty items"
        [ test "shows no empty node when the item has a visible question" <|
            \_ ->
                emptyNodeFor listWithVisibleUuid QuestionnaireViewSettings.none
                    |> Expect.equal Nothing
        , test "shows a no-questions empty node when the item template has no questions" <|
            \_ ->
                emptyNodeFor listNoQuestionsUuid QuestionnaireViewSettings.none
                    |> Maybe.map .hiddenByViewOptions
                    |> Expect.equal (Just False)
        , test "shows a hidden-by-view-options empty node when all questions are hidden" <|
            \_ ->
                emptyNodeFor listAllHiddenUuid QuestionnaireViewSettings.none
                    |> Maybe.map .hiddenByViewOptions
                    |> Expect.equal (Just True)
        , test "shows no empty node for the hidden list when non-desirable questions are visible" <|
            \_ ->
                emptyNodeFor listAllHiddenUuid QuestionnaireViewSettings.all
                    |> Expect.equal Nothing
        ]
