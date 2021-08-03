module Wizard.KnowledgeModels.Preview.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import Dict.Extra as Dict
import Json.Encode as E
import Json.Encode.Extra as E
import Random exposing (Seed)
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.PackageDetail as PackageDetail
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent(..))
import Shared.Data.QuestionnaireDetail.Reply exposing (Reply)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setKnowledgeModel, setPackage)
import Shared.Utils exposing (getUuid, getUuidString)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState packageId =
    Cmd.batch
        [ KnowledgeModelsApi.fetchPreview (Just packageId) [] [] appState FetchPreviewComplete
        , PackagesApi.getPackage packageId appState GetPackageComplete
        ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FetchPreviewComplete result ->
            initQuestionnaireModel appState <|
                applyResult appState
                    { setResult = setKnowledgeModel
                    , defaultError = lg "apiError.knowledgeModels.preview.fetchError" appState
                    , model = model
                    , result = result
                    }

        GetPackageComplete result ->
            initQuestionnaireModel appState <|
                applyResult appState
                    { setResult = setPackage
                    , defaultError = lg "apiError.packages.getListError" appState
                    , model = model
                    , result = result
                    }

        QuestionnaireMsg qtnMsg ->
            handleQuestionnaireMsg qtnMsg wrapMsg appState model

        CreateProjectMsg ->
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        body =
                            E.object
                                [ ( "name", E.string questionnaireModel.questionnaire.package.name )
                                , ( "packageId", E.string questionnaireModel.questionnaire.package.id )
                                , ( "visibility", QuestionnaireVisibility.encode appState.config.questionnaire.questionnaireVisibility.defaultValue )
                                , ( "sharing", QuestionnaireSharing.encode QuestionnaireSharing.AnyoneWithLinkEditQuestionnaire )
                                , ( "tagUuids", E.list E.string [] )
                                , ( "templateId", E.maybe E.string Nothing )
                                ]

                        cmd =
                            Cmd.map wrapMsg <|
                                QuestionnairesApi.postQuestionnaire body appState PostQuestionnaireCompleted
                    in
                    ( appState.seed, { model | creatingQuestionnaire = Loading }, cmd )

                _ ->
                    ( appState.seed, model, Cmd.none )

        PostQuestionnaireCompleted result ->
            case result of
                Ok questionnaire ->
                    case ( ActionResult.unwrap True (.questionnaire >> .replies >> Dict.isEmpty) model.questionnaireModel, model.questionnaireModel ) of
                        ( False, Success questionnaireModel ) ->
                            let
                                toEvent ( path, reply ) ( seed, list ) =
                                    let
                                        ( uuid, nextSeed ) =
                                            getUuid seed

                                        event =
                                            SetReply
                                                { uuid = uuid
                                                , path = path
                                                , value = reply.value
                                                , createdAt = reply.createdAt
                                                , createdBy = reply.createdBy
                                                }
                                    in
                                    ( nextSeed, event :: list )

                                ( newSeed, events ) =
                                    Dict.toList questionnaireModel.questionnaire.replies
                                        |> List.sortBy (Tuple.first >> String.length)
                                        |> List.foldr toEvent ( appState.seed, [] )

                                cmd =
                                    Cmd.map wrapMsg <|
                                        QuestionnairesApi.putQuestionnaireContent questionnaire.uuid events appState (PutQuestionnaireContentComplete questionnaire.uuid)
                            in
                            ( newSeed, model, cmd )

                        _ ->
                            ( appState.seed, model, cmdNavigate appState (Wizard.Routes.ProjectsRoute (DetailRoute questionnaire.uuid PlanDetailRoute.Questionnaire)) )

                Err error ->
                    ( appState.seed
                    , { model | creatingQuestionnaire = ApiError.toActionResult appState (lg "apiError.questionnaires.postError" appState) error }
                    , Cmd.none
                    )

        PutQuestionnaireContentComplete questionnaireUuid result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, cmdNavigate appState (Wizard.Routes.ProjectsRoute (DetailRoute questionnaireUuid PlanDetailRoute.Questionnaire)) )

                Err error ->
                    ( appState.seed
                    , { model | creatingQuestionnaire = ApiError.toActionResult appState (lg "apiError.questionnaires.putError" appState) error }
                    , Cmd.none
                    )


initQuestionnaireModel : AppState -> ( Model, Cmd Wizard.Msgs.Msg ) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
initQuestionnaireModel appState ( model, cmd ) =
    case ActionResult.combine model.knowledgeModel model.package of
        Success ( knowledgeModel, package ) ->
            let
                questionnaire =
                    QuestionnaireDetail.createQuestionnaireDetail (PackageDetail.toPackage package) knowledgeModel

                ( ( newSeed, mbChapterUuid, questionnaireWithReplies ), scrollCmd ) =
                    case model.mbQuestionUuid of
                        Just questionUuid ->
                            ( generateReplies appState questionUuid knowledgeModel questionnaire
                            , Ports.scrollIntoView ("#question-" ++ questionUuid)
                            )

                        _ ->
                            ( ( appState.seed, Nothing, questionnaire ), Cmd.none )

                questionnaireModel =
                    Questionnaire.init appState questionnaireWithReplies

                questionnaireModelWithChapter =
                    case mbChapterUuid of
                        Just chapterUuid ->
                            Questionnaire.setActiveChapterUuid chapterUuid questionnaireModel

                        Nothing ->
                            questionnaireModel
            in
            ( newSeed, { model | questionnaireModel = Success questionnaireModelWithChapter }, Cmd.batch [ cmd, scrollCmd ] )

        Error err ->
            ( appState.seed, { model | questionnaireModel = Error err }, cmd )

        _ ->
            ( appState.seed, model, cmd )


generateReplies : AppState -> String -> KnowledgeModel -> QuestionnaireDetail -> ( Seed, Maybe String, QuestionnaireDetail )
generateReplies appState questionUuid km questionnaireDetail =
    let
        parentMap =
            KnowledgeModel.createParentMap km

        ( newSeed, mbChapterUuid, replies ) =
            foldReplies appState km parentMap appState.seed questionUuid Dict.empty
    in
    ( newSeed
    , mbChapterUuid
    , { questionnaireDetail | replies = replies }
    )


foldReplies : AppState -> KnowledgeModel -> KnowledgeModel.ParentMap -> Seed -> String -> Dict String Reply -> ( Seed, Maybe String, Dict String Reply )
foldReplies appState km parentMap seed questionUuid replies =
    let
        parentUuid =
            KnowledgeModel.getParent parentMap questionUuid

        prefixPaths prefix repliesDict =
            Dict.mapKeys (\k -> prefix ++ "." ++ k) repliesDict

        foldReplies_ =
            foldReplies appState km parentMap
    in
    case
        ( KnowledgeModel.getChapter parentUuid km
        , KnowledgeModel.getQuestion parentUuid km
        , KnowledgeModel.getAnswer parentUuid km
        )
    of
        ( Just chapter, Nothing, Nothing ) ->
            -- just prefix replies with chapter uuid
            ( seed, Just chapter.uuid, prefixPaths chapter.uuid replies )

        ( Nothing, Just question, Nothing ) ->
            -- add item to question, get parent question and continue
            let
                ( itemUuid, newSeed ) =
                    getUuidString seed

                reply =
                    { value = ReplyValue.ItemListReply [ itemUuid ]
                    , createdAt = appState.currentTime
                    , createdBy = Nothing
                    }

                listQuestionUuid =
                    Question.getUuid question
            in
            foldReplies_ newSeed
                listQuestionUuid
                (Dict.insert listQuestionUuid reply (prefixPaths listQuestionUuid (prefixPaths itemUuid replies)))

        ( Nothing, Nothing, Just answer ) ->
            -- select answer, get parent question and continue
            let
                answerParentQuestionUuid =
                    KnowledgeModel.getParent parentMap answer.uuid
            in
            case KnowledgeModel.getQuestion answerParentQuestionUuid km of
                Just question ->
                    let
                        reply =
                            { value = ReplyValue.AnswerReply answer.uuid
                            , createdAt = appState.currentTime
                            , createdBy = Nothing
                            }

                        answerQuestionUuid =
                            Question.getUuid question
                    in
                    foldReplies_ seed
                        answerQuestionUuid
                        (Dict.insert answerQuestionUuid reply (prefixPaths answerQuestionUuid (prefixPaths answer.uuid replies)))

                Nothing ->
                    -- should not happen
                    ( seed, Nothing, replies )

        _ ->
            -- should not happen
            ( seed, Nothing, replies )


handleQuestionnaireMsg : Questionnaire.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleQuestionnaireMsg msg wrapMsg appState model =
    case model.questionnaireModel of
        Success questionnaireModel ->
            let
                ( newSeed, qm, qtnCmd ) =
                    Questionnaire.update msg
                        QuestionnaireMsg
                        Nothing
                        appState
                        { events = []
                        }
                        questionnaireModel
            in
            ( newSeed, { model | questionnaireModel = Success qm }, Cmd.map wrapMsg qtnCmd )

        _ ->
            ( appState.seed, model, Cmd.none )
