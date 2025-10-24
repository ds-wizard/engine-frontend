module Wizard.Pages.KnowledgeModels.Preview.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError
import Common.Ports.Dom as Dom
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModel, setPackage)
import Dict
import Gettext exposing (gettext)
import Json.Encode as E
import Json.Encode.Extra as E
import Random exposing (Seed)
import Uuid.Extra as Uuid
import Wizard.Api.KnowledgeModels as KnowledgeModelsApi
import Wizard.Api.Models.PackageDetail as PackageDetail
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent(..))
import Wizard.Api.Models.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire
import Wizard.Api.Packages as PackagesApi
import Wizard.Api.Questionnaires as QuestionnairesApi
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Preview.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Preview.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> String -> Cmd Msg
fetchData appState packageId =
    Cmd.batch
        [ KnowledgeModelsApi.fetchPreview appState (Just packageId) [] [] FetchPreviewComplete
        , PackagesApi.getPackage appState packageId GetPackageComplete
        ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FetchPreviewComplete result ->
            initQuestionnaireModel appState <|
                RequestHelpers.applyResult
                    { setResult = setKnowledgeModel
                    , defaultError = gettext "Unable to get the knowledge model." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    , locale = appState.locale
                    }

        GetPackageComplete result ->
            initQuestionnaireModel appState <|
                RequestHelpers.applyResult
                    { setResult = setPackage
                    , defaultError = gettext "Unable to get knowledge models." appState.locale
                    , model = model
                    , result = result
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    , locale = appState.locale
                    }

        QuestionnaireMsg qtnMsg ->
            handleQuestionnaireMsg qtnMsg wrapMsg appState model

        CreateProjectMsg ->
            case model.package of
                Success package ->
                    let
                        body =
                            E.object
                                [ ( "name", E.string package.name )
                                , ( "packageId", E.string package.id )
                                , ( "visibility", QuestionnaireVisibility.encode appState.config.questionnaire.questionnaireVisibility.defaultValue )
                                , ( "sharing", QuestionnaireSharing.encode QuestionnaireSharing.AnyoneWithLinkEditQuestionnaire )
                                , ( "questionTagUuids", E.list E.string [] )
                                , ( "templateId", E.maybe E.string Nothing )
                                ]

                        cmd =
                            Cmd.map wrapMsg <|
                                QuestionnairesApi.postQuestionnaire appState body PostQuestionnaireCompleted
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
                                            Uuid.step seed

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
                                        QuestionnairesApi.putQuestionnaireContent appState questionnaire.uuid events (PutQuestionnaireContentComplete questionnaire.uuid)
                            in
                            ( newSeed, model, cmd )

                        _ ->
                            ( appState.seed, model, cmdNavigate appState (Routes.projectsDetail questionnaire.uuid) )

                Err error ->
                    ( appState.seed
                    , { model | creatingQuestionnaire = ApiError.toActionResult appState (gettext "Questionnaire could not be created." appState.locale) error }
                    , Cmd.none
                    )

        PutQuestionnaireContentComplete questionnaireUuid result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, cmdNavigate appState (Routes.projectsDetail questionnaireUuid) )

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
                    QuestionnaireQuestionnaire.createQuestionnaireDetail (PackageDetail.toPackage package) knowledgeModel

                ( ( newSeed, mbChapterUuid, questionnaireWithReplies ), scrollCmd ) =
                    case model.mbQuestionUuid of
                        Just questionUuid ->
                            ( QuestionnaireQuestionnaire.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel questionnaire
                            , Dom.scrollIntoView ("#question-" ++ questionUuid)
                            )

                        _ ->
                            ( ( appState.seed, Nothing, questionnaire ), Cmd.none )

                ( questionnaireModel, _ ) =
                    Questionnaire.initSimple appState questionnaireWithReplies

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
                        , kmEditorUuid = Nothing
                        }
                        questionnaireModel
            in
            ( newSeed, { model | questionnaireModel = Success qm }, Cmd.map wrapMsg qtnCmd )

        _ ->
            ( appState.seed, model, Cmd.none )
