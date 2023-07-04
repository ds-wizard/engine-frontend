module Wizard.KnowledgeModels.Preview.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Dict
import Gettext exposing (gettext)
import Json.Encode as E
import Json.Encode.Extra as E
import Random exposing (Seed)
import Shared.Api.KnowledgeModels as KnowledgeModelsApi
import Shared.Api.Packages as PackagesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.PackageDetail as PackageDetail
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent(..))
import Shared.Data.SummaryReport.AnsweredIndicationData as AnsweredIndicationData
import Shared.Error.ApiError as ApiError
import Shared.Setters exposing (setKnowledgeModel, setPackage)
import Shared.Utils exposing (getUuid)
import Wizard.Common.Api exposing (applyResult)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
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
                    , defaultError = gettext "Unable to get the Knowledge Model." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    }

        GetPackageComplete result ->
            initQuestionnaireModel appState <|
                applyResult appState
                    { setResult = setPackage
                    , defaultError = gettext "Unable to get Knowledge Models." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = Wizard.Msgs.logoutMsg
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
                                , ( "questionTagUuids", E.list E.string [] )
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
                                                , phasesAnsweredIndication = AnsweredIndicationData.empty
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
                            ( appState.seed, model, cmdNavigate appState (Routes.projectsDetailQuestionnaire questionnaire.uuid Nothing) )

                Err error ->
                    ( appState.seed
                    , { model | creatingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be created." appState.locale) error }
                    , Cmd.none
                    )

        PutQuestionnaireContentComplete questionnaireUuid result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, cmdNavigate appState (Routes.projectsDetailQuestionnaire questionnaireUuid Nothing) )

                Err error ->
                    ( appState.seed
                    , { model | creatingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
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
                            ( QuestionnaireDetail.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel questionnaire
                            , Ports.scrollIntoView ("#question-" ++ questionUuid)
                            )

                        _ ->
                            ( ( appState.seed, Nothing, questionnaire ), Cmd.none )

                ( questionnaireModel, _ ) =
                    Questionnaire.init appState questionnaireWithReplies Nothing

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
