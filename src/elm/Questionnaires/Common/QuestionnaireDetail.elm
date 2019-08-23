module Questionnaires.Common.QuestionnaireDetail exposing
    ( QuestionnaireDetail
    , decoder
    , encode
    , getTodos
    , setLevel
    , todosLength
    , updateLabels
    , updateReplies
    )

import Common.FormEngine.Model exposing (FormValue, FormValues, decodeFormValues, encodeFormValues, getAnswerUuid, getItemListCount)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import KnowledgeModels.Common.Package as Package exposing (Package)
import List.Extra as List
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
import Questionnaires.Common.QuestionnaireLabel as QuestionnaireLabel exposing (QuestionnaireLabel)
import Questionnaires.Common.QuestionnaireTodo exposing (QuestionnaireTodo)


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
        |> D.required "knowledgeModel" KnowledgeModel.decoder
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


todosLength : QuestionnaireDetail -> Int
todosLength =
    List.length << getTodos


getTodos : QuestionnaireDetail -> List QuestionnaireTodo
getTodos questionnaire =
    List.concatMap
        (getChapterTodos questionnaire)
        (KnowledgeModel.getChapters questionnaire.knowledgeModel)


getChapterTodos : QuestionnaireDetail -> Chapter -> List QuestionnaireTodo
getChapterTodos questionnaire chapter =
    List.concatMap
        (getQuestionTodos questionnaire chapter [ chapter.uuid ])
        (KnowledgeModel.getChapterQuestions chapter.uuid questionnaire.knowledgeModel)


getQuestionTodos : QuestionnaireDetail -> Chapter -> List String -> Question -> List QuestionnaireTodo
getQuestionTodos questionnaire chapter path question =
    let
        km =
            questionnaire.knowledgeModel

        currentPath =
            path ++ [ Question.getUuid question ]

        questionTodo =
            if hasTodo questionnaire (pathToString currentPath) then
                [ { chapter = chapter
                  , question = question
                  , path = pathToString currentPath
                  }
                ]

            else
                []

        childTodos =
            case getReply questionnaire (pathToString currentPath) of
                Just formValue ->
                    case question of
                        OptionsQuestion commonData _ ->
                            case List.find (.uuid >> (==) (getAnswerUuid formValue.value)) (KnowledgeModel.getQuestionAnswers commonData.uuid km) of
                                Just answer ->
                                    List.concatMap
                                        (getQuestionTodos questionnaire chapter (currentPath ++ [ answer.uuid ]))
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km)

                                Nothing ->
                                    []

                        ListQuestion commonData _ ->
                            let
                                getItemQuestionTodos index =
                                    List.concatMap
                                        (getQuestionTodos questionnaire chapter (currentPath ++ [ String.fromInt index ]))
                                        (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid km)
                            in
                            List.range 0 (getItemListCount formValue.value)
                                |> List.concatMap getItemQuestionTodos

                        _ ->
                            []

                Nothing ->
                    []
    in
    questionTodo ++ childTodos


getReply : QuestionnaireDetail -> String -> Maybe FormValue
getReply questionnaire path =
    List.find (.path >> (==) path) questionnaire.replies


hasTodo : QuestionnaireDetail -> String -> Bool
hasTodo questionnaire path =
    List.any (.path >> (==) path) questionnaire.labels


pathToString : List String -> String
pathToString =
    String.join "."
