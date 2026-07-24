module Wizard.Api.Models.ProjectQuestionnaireTest exposing (generateRepliesTest)

import Dict exposing (Dict)
import Expect
import Json.Decode as D
import Random
import Test exposing (Test, describe, test)
import Time
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Wizard.Api.Models.ProjectDetail.Reply exposing (Reply)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue(..))
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire


chapterUuid : String
chapterUuid =
    "11111111-1111-1111-1111-111111111111"


listQuestionUuid : String
listQuestionUuid =
    "22222222-2222-2222-2222-222222222222"


itemQuestionUuid : String
itemQuestionUuid =
    "33333333-3333-3333-3333-333333333333"


optionsQuestionUuid : String
optionsQuestionUuid =
    "44444444-4444-4444-4444-444444444444"


answer1Uuid : String
answer1Uuid =
    "55555555-5555-5555-5555-555555555555"


answer2Uuid : String
answer2Uuid =
    "66666666-6666-6666-6666-666666666666"


followUpQuestionUuid : String
followUpQuestionUuid =
    "77777777-7777-7777-7777-777777777777"


savedItemUuid : String
savedItemUuid =
    "88888888-8888-8888-8888-888888888888"


{-| Chapter → List question (with item template value question) and
Chapter → Options question (answer 1 leads to a follow-up value question).
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
                    "questionUuids": ["22222222-2222-2222-2222-222222222222", "44444444-4444-4444-4444-444444444444"],
                    "annotations": []
                }
            },
            "questions": {
                "22222222-2222-2222-2222-222222222222": {
                    "uuid": "22222222-2222-2222-2222-222222222222",
                    "questionType": "ListQuestion",
                    "title": "List question",
                    "text": null,
                    "requiredPhaseUuid": null,
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
                    "title": "Item question",
                    "text": null,
                    "requiredPhaseUuid": null,
                    "tagUuids": [],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                },
                "44444444-4444-4444-4444-444444444444": {
                    "uuid": "44444444-4444-4444-4444-444444444444",
                    "questionType": "OptionsQuestion",
                    "title": "Options question",
                    "text": null,
                    "requiredPhaseUuid": null,
                    "tagUuids": [],
                    "answerUuids": ["55555555-5555-5555-5555-555555555555", "66666666-6666-6666-6666-666666666666"],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                },
                "77777777-7777-7777-7777-777777777777": {
                    "uuid": "77777777-7777-7777-7777-777777777777",
                    "questionType": "ValueQuestion",
                    "valueType": "StringQuestionValueType",
                    "validations": [],
                    "title": "Follow-up question",
                    "text": null,
                    "requiredPhaseUuid": null,
                    "tagUuids": [],
                    "referenceUuids": [],
                    "expertUuids": [],
                    "annotations": []
                }
            },
            "answers": {
                "55555555-5555-5555-5555-555555555555": {
                    "uuid": "55555555-5555-5555-5555-555555555555",
                    "label": "Answer 1",
                    "advice": null,
                    "metricMeasures": [],
                    "followUpUuids": ["77777777-7777-7777-7777-777777777777"],
                    "annotations": []
                },
                "66666666-6666-6666-6666-666666666666": {
                    "uuid": "66666666-6666-6666-6666-666666666666",
                    "label": "Answer 2",
                    "advice": null,
                    "metricMeasures": [],
                    "followUpUuids": [],
                    "annotations": []
                }
            },
            "choices": {},
            "experts": {},
            "references": {},
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


reply : ReplyValue -> Reply
reply value =
    { value = value, createdAt = Time.millisToPosix 0, createdBy = Nothing }


generate : Dict String Reply -> String -> Dict String Reply
generate savedReplies questionUuid =
    let
        questionnaire =
            ProjectQuestionnaire.createQuestionnaireDetail KnowledgeModelPackage.dummy knowledgeModel

        ( _, _, resultQuestionnaire ) =
            ProjectQuestionnaire.generateReplies (Time.millisToPosix 0)
                (Random.initialSeed 0)
                questionUuid
                knowledgeModel
                { questionnaire | replies = savedReplies }
    in
    resultQuestionnaire.replies


valueAt : String -> Dict String Reply -> Maybe ReplyValue
valueAt key replies =
    Maybe.map .value (Dict.get key replies)


generateRepliesTest : Test
generateRepliesTest =
    describe "ProjectQuestionnaire.generateReplies"
        [ test "preserves saved items and nested item replies when opening a question inside a list" <|
            \_ ->
                let
                    listKey =
                        chapterUuid ++ "." ++ listQuestionUuid

                    nestedKey =
                        listKey ++ "." ++ savedItemUuid ++ "." ++ itemQuestionUuid

                    savedReplies =
                        Dict.fromList
                            [ ( listKey, reply (ItemListReply [ savedItemUuid ]) )
                            , ( nestedKey, reply (StringReply "saved value") )
                            ]

                    result =
                        generate savedReplies itemQuestionUuid
                in
                Expect.all
                    [ \r -> Expect.equal (Just (ItemListReply [ savedItemUuid ])) (valueAt listKey r)
                    , \r -> Expect.equal (Just (StringReply "saved value")) (valueAt nestedKey r)
                    ]
                    result
        , test "adds a single new item when the list has no saved items" <|
            \_ ->
                let
                    listKey =
                        chapterUuid ++ "." ++ listQuestionUuid

                    result =
                        generate Dict.empty itemQuestionUuid

                    itemCount =
                        valueAt listKey result
                            |> Maybe.map ReplyValue.getItemUuids
                            |> Maybe.map List.length
                in
                Expect.equal (Just 1) itemCount
        , test "keeps the already selected answer and its follow-up replies" <|
            \_ ->
                let
                    optionsKey =
                        chapterUuid ++ "." ++ optionsQuestionUuid

                    followUpKey =
                        optionsKey ++ "." ++ answer1Uuid ++ "." ++ followUpQuestionUuid

                    savedReplies =
                        Dict.fromList
                            [ ( optionsKey, reply (AnswerReply answer1Uuid) )
                            , ( followUpKey, reply (StringReply "saved follow-up") )
                            ]

                    result =
                        generate savedReplies followUpQuestionUuid
                in
                Expect.all
                    [ \r -> Expect.equal (Just (AnswerReply answer1Uuid)) (valueAt optionsKey r)
                    , \r -> Expect.equal (Just (StringReply "saved follow-up")) (valueAt followUpKey r)
                    ]
                    result
        , test "switches the selected answer when a different one is saved" <|
            \_ ->
                let
                    optionsKey =
                        chapterUuid ++ "." ++ optionsQuestionUuid

                    savedReplies =
                        Dict.fromList
                            [ ( optionsKey, reply (AnswerReply answer2Uuid) ) ]

                    result =
                        generate savedReplies followUpQuestionUuid
                in
                Expect.equal (Just (AnswerReply answer1Uuid)) (valueAt optionsKey result)
        ]
