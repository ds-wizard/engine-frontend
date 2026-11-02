module Common.Pages.KnowledgeModels.Preview exposing
    ( Model
    , Msg
    , init
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| A knowledge model package, previewed as the questionnaire it makes.

Read-only, with no toolbar and no actions on the questions. With `?questionUuid`
the preview opens on that question, with the answers that lead to it filled in
(`ProjectQuestionnaire.generateReplies`) and the question scrolled into view by
its path. "Create project" makes a project of the package and gives it the
preview's replies.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Common.Api.KnowledgeModels as KnowledgeModelsApi
import Common.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.Project exposing (Project)
import Common.Api.Models.Project.ProjectSharing as ProjectSharing
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility
import Common.Api.Models.ProjectCommon as ProjectCommon
import Common.Api.Models.ProjectDetail.ProjectEvent exposing (ProjectEvent(..))
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire
import Common.Api.Projects as ProjectsApi
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.DetailPage as DetailPage
import Common.Components.Page as Page
import Common.Components.Questionnaire as Questionnaire
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.NotFound as NotFound
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Dict
import Gettext exposing (gettext)
import Html exposing (Html)
import Json.Encode as E
import Json.Encode.Extra as E
import Random exposing (Seed)
import Ui.Button as Button
import Ui.Icon as Icon
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Version


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , knowledgeModelPackage : ActionResult KnowledgeModelPackageDetail
    , questionnaireModel : ActionResult Questionnaire.Model
    , mbQuestionUuid : Maybe String
    , creatingProject : ActionResult Project
    }


initialModel : Maybe String -> Model
initialModel mbQuestionUuid =
    { knowledgeModel = Loading
    , knowledgeModelPackage = Loading
    , questionnaireModel = Loading
    , mbQuestionUuid = mbQuestionUuid
    , creatingProject = Unset
    }


init : AppState -> Uuid -> Maybe String -> ( Model, Cmd Msg )
init appState kmPackageUuid mbQuestionUuid =
    ( initialModel mbQuestionUuid
    , Cmd.batch
        [ KnowledgeModelsApi.fetchPreview appState (Just kmPackageUuid) [] [] FetchPreviewComplete
        , KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmPackageUuid GetPackageComplete
        ]
    )


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
    | GetPackageComplete (Result ApiError KnowledgeModelPackageDetail)
    | QuestionnaireMsg Questionnaire.Msg
    | CreateProject
    | PostProjectCompleted (Result ApiError Project)
    | PutContentCompleted Uuid (Result ApiError ())


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FetchPreviewComplete result ->
            initQuestionnaire cfg appState <|
                RequestHelpers.applyResultOrNotFound
                    { setResult = \value m -> { m | knowledgeModel = value }
                    , defaultError = gettext "Unable to get the knowledge model." appState.locale
                    , notFoundCmd = UpdateConfig.notFound cfg (NotFound.knowledgeModel appState)
                    , notAllowedCmd = UpdateConfig.notAllowed cfg
                    , model = model
                    , result = result
                    , logoutMsg = cfg.logoutMsg
                    , locale = appState.locale
                    }

        GetPackageComplete result ->
            initQuestionnaire cfg appState <|
                RequestHelpers.applyResultOrNotFound
                    { setResult = \value m -> { m | knowledgeModelPackage = value }
                    , defaultError = gettext "Unable to get knowledge models." appState.locale
                    , notFoundCmd = UpdateConfig.notFound cfg (NotFound.knowledgeModel appState)
                    , notAllowedCmd = UpdateConfig.notAllowed cfg
                    , model = model
                    , result = result
                    , logoutMsg = cfg.logoutMsg
                    , locale = appState.locale
                    }

        QuestionnaireMsg questionnaireMsg ->
            case model.questionnaireModel of
                Success questionnaireModel ->
                    let
                        result =
                            Questionnaire.update appState (questionnaireConfig cfg) questionnaireMsg questionnaireModel
                    in
                    ( result.seed, { model | questionnaireModel = Success result.model }, result.cmd )

                _ ->
                    ( appState.seed, model, Cmd.none )

        CreateProject ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    let
                        body =
                            E.object
                                [ ( "name", E.string kmPackage.name )
                                , ( "knowledgeModelPackageUuid", Uuid.encode kmPackage.uuid )
                                , ( "visibility", ProjectVisibility.encode appState.config.project.projectVisibility.defaultValue )
                                , ( "sharing", ProjectSharing.encode ProjectSharing.AnyoneWithLinkEdit )
                                , ( "questionTagUuids", E.list E.string [] )
                                , ( "projectUuid", E.maybe E.string Nothing )
                                ]
                    in
                    ( appState.seed
                    , { model | creatingProject = Loading }
                    , Cmd.map cfg.wrapMsg (ProjectsApi.post appState body PostProjectCompleted)
                    )

                _ ->
                    ( appState.seed, model, Cmd.none )

        PostProjectCompleted result ->
            case ( result, model.questionnaireModel ) of
                ( Ok project, Success questionnaireModel ) ->
                    let
                        replies =
                            questionnaireModel.questionnaire.replies
                    in
                    if Dict.isEmpty replies then
                        ( appState.seed, model, Routes.navigate appState (Routes.projectsDetail project.uuid) )

                    else
                        let
                            -- The replies as the events a project is written with,
                            -- the shorter paths first so each parent is there
                            -- before its children.
                            toEvent ( path, reply ) ( currentSeed, acc ) =
                                let
                                    ( uuid, nextSeed ) =
                                        Uuid.step currentSeed
                                in
                                ( nextSeed
                                , SetReply { uuid = uuid, path = path, value = reply.value, createdAt = reply.createdAt, createdBy = reply.createdBy } :: acc
                                )

                            ( seed, events ) =
                                Dict.toList replies
                                    |> List.sortBy (Tuple.first >> String.length)
                                    |> List.foldr toEvent ( appState.seed, [] )
                        in
                        ( seed, model, Cmd.map cfg.wrapMsg (ProjectsApi.putContent appState project.uuid events (PutContentCompleted project.uuid)) )

                ( Ok project, _ ) ->
                    ( appState.seed, model, Routes.navigate appState (Routes.projectsDetail project.uuid) )

                ( Err error, _ ) ->
                    ( appState.seed
                    , { model | creatingProject = ApiError.toActionResult appState (gettext "Questionnaire could not be created." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        PutContentCompleted projectUuid result ->
            case result of
                Ok _ ->
                    ( appState.seed, model, Routes.navigate appState (Routes.projectsDetail projectUuid) )

                Err error ->
                    ( appState.seed
                    , { model | creatingProject = ApiError.toActionResult appState (gettext "Questionnaire could not be saved." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


questionnaireConfig : UpdateConfig Msg msg -> Questionnaire.UpdateConfig msg
questionnaireConfig cfg =
    { wrapMsg = cfg.wrapMsg << QuestionnaireMsg
    , mbKmEditorUuid = Nothing
    , previewVersionMsg = Nothing
    , revertVersionMsg = Nothing
    , projectCommon = ProjectCommon.dummy
    }


{-| Once both the knowledge model and the package are here, the questionnaire —
opened on the linked question, when there is one.
-}
initQuestionnaire : UpdateConfig Msg msg -> AppState -> ( Model, Cmd msg ) -> ( Seed, Model, Cmd msg )
initQuestionnaire cfg appState ( model, cmd ) =
    case ActionResult.combine model.knowledgeModel model.knowledgeModelPackage of
        Success ( knowledgeModel, kmPackage ) ->
            let
                questionnaire =
                    ProjectQuestionnaire.createQuestionnaireDetail (KnowledgeModelPackageDetail.toPackage kmPackage) knowledgeModel

                ( seed, mbPath, withReplies ) =
                    case model.mbQuestionUuid of
                        Just questionUuid ->
                            let
                                ( nextSeed, _, generated ) =
                                    ProjectQuestionnaire.generateReplies appState.currentTime appState.seed questionUuid knowledgeModel questionnaire
                            in
                            ( nextSeed, ProjectQuestionnaire.getClosestQuestionParentPath generated (Common.Api.Models.KnowledgeModel.createParentMap knowledgeModel) questionUuid, generated )

                        Nothing ->
                            ( appState.seed, Nothing, questionnaire )

                ( questionnaireModel, questionnaireCmd ) =
                    Questionnaire.init appState withReplies mbPath Nothing
            in
            ( seed
            , { model | questionnaireModel = Success questionnaireModel }
            , Cmd.batch [ cmd, Cmd.map (cfg.wrapMsg << QuestionnaireMsg) questionnaireCmd ]
            )

        Error error ->
            ( appState.seed, { model | questionnaireModel = Error error }, cmd )

        _ ->
            ( appState.seed, model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaireModel of
        Success questionnaireModel ->
            Sub.map QuestionnaireMsg (Questionnaire.subscriptions questionnaireModel)

        _ ->
            Sub.none


view : RowPolicy -> AppState -> Model -> Html Msg
view policy appState model =
    Page.actionResult appState (viewPreview policy appState model) (ActionResult.combine model.knowledgeModelPackage model.questionnaireModel)


viewPreview : RowPolicy -> AppState -> Model -> ( KnowledgeModelPackageDetail, Questionnaire.Model ) -> Html Msg
viewPreview policy appState model ( kmPackage, questionnaireModel ) =
    let
        actions =
            if Feature.projectsCreateCustom appState && policy.derive then
                [ Button.primary (gettext "Create project" appState.locale) CreateProject
                    |> Button.withIcon (Icon.solid "fa-plus")
                    |> Button.withLoading (ActionResult.isLoading model.creatingProject)
                    |> Button.withTestId "km-preview_create-project"
                ]

            else
                []
    in
    -- A detail page with flush content: the header stays, and the questionnaire's
    -- workspace fills the rest, scrolling its regions on its own. An error in
    -- creating the project goes after it, in a row that sizes to what it holds.
    DetailPage.detailPage
        { title = kmPackage.name ++ ", " ++ Version.toString kmPackage.version
        , content =
            [ Questionnaire.view appState
                { wrapMsg = QuestionnaireMsg
                , readonly = True
                , commentsReadonly = True
                , toolbarEnabled = False
                , actionsEnabled = False
                , canViewVersions = False
                , canRevert = False
                }
                questionnaireModel
            , ApiErrorAlert.view appState model.creatingProject
            ]
        }
        |> DetailPage.withActions actions
        |> DetailPage.withFlushContent True
        |> DetailPage.withLocale appState
        |> DetailPage.toHtml
