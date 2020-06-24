module Shared.Data.QuestionnaireDetail exposing
    ( QuestionnaireDetail
    , decoder
    , encode
    , getTodos
    , isEditable
    , setLevel
    , todosLength
    , updateLabels
    , updateReplies
    )

-- TODO: Move FormEngine to Shared

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.Questionnaire.QuestionnaireLabel as QuestionnaireLabel exposing (QuestionnaireLabel)
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Data.QuestionnaireDetail.FormValue as FormValue exposing (FormValue)
import Shared.Data.QuestionnaireDetail.FormValue.ReplyValue as ReplyValue
import Shared.Data.UserInfo as UserInfo
import Uuid exposing (Uuid)


type alias QuestionnaireDetail =
    { uuid : Uuid
    , name : String
    , package : Package
    , knowledgeModel : KnowledgeModel
    , replies : List FormValue
    , level : Int
    , visibility : QuestionnaireVisibility
    , ownerUuid : Maybe Uuid
    , selectedTagUuids : List String
    , labels : List QuestionnaireLabel
    }


decoder : Decoder QuestionnaireDetail
decoder =
    D.succeed QuestionnaireDetail
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "package" Package.decoder
        |> D.required "knowledgeModel" KnowledgeModel.decoder
        |> D.required "replies" (D.list FormValue.decoder)
        |> D.required "level" D.int
        |> D.required "visibility" QuestionnaireVisibility.decoder
        |> D.required "ownerUuid" (D.maybe Uuid.decoder)
        |> D.required "selectedTagUuids" (D.list D.string)
        |> D.required "labels" (D.list QuestionnaireLabel.decoder)


encode : QuestionnaireDetail -> E.Value
encode questionnaire =
    E.object
        [ ( "name", E.string questionnaire.name )
        , ( "visibility", QuestionnaireVisibility.encode questionnaire.visibility )
        , ( "replies", E.list FormValue.encode questionnaire.replies )
        , ( "level", E.int questionnaire.level )
        , ( "labels", E.list QuestionnaireLabel.encode questionnaire.labels )
        ]


isEditable : AbstractAppState a -> QuestionnaireDetail -> Bool
isEditable appState questionnaire =
    let
        isAdmin =
            UserInfo.isAdmin appState.session.user

        isNotReadonly =
            questionnaire.visibility /= PublicReadOnlyQuestionnaire

        isOwner =
            questionnaire.ownerUuid == Maybe.map .uuid appState.session.user
    in
    isAdmin || isNotReadonly || isOwner


updateReplies : List FormValue -> QuestionnaireDetail -> QuestionnaireDetail
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
                            case List.find (.uuid >> (==) (ReplyValue.getAnswerUuid formValue.value)) (KnowledgeModel.getQuestionAnswers commonData.uuid km) of
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
                            List.range 0 (ReplyValue.getItemListCount formValue.value)
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
