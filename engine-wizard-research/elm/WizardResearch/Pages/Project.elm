module WizardResearch.Pages.Project exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html.Styled as Html exposing (Html)
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Elemental.Components.ActionResultWrapper as ActionResultWrapper
import Shared.Elemental.Components.SideNavigation as SideNavigation
import Shared.Elemental.Foundations.Animation as Animation
import Shared.Error.ApiError as ApiError exposing (ApiError)
import WizardResearch.Common.AppState exposing (AppState)
import WizardResearch.Components.ProjectMenu as ProjectMenu
import WizardResearch.Pages.Project.Documents as Documents
import WizardResearch.Pages.Project.Metrics as Metrics
import WizardResearch.Pages.Project.Overview as Overview
import WizardResearch.Pages.Project.Planning as Planning
import WizardResearch.Pages.Project.Settings as Settings
import WizardResearch.Pages.Project.Starred as Starred
import WizardResearch.Route.ProjectRoute as ProjectRoute exposing (ProjectRoute)



-- MODEL


type alias Model =
    { questionnaire : ActionResult Questionnaire
    , pageModel : PageModel
    }


type PageModel
    = Overview Overview.Model
    | Planning Planning.Model
    | Starred Starred.Model
    | Metrics Metrics.Model
    | Documents Documents.Model
    | Settings Settings.Model


init : AppState -> String -> ProjectRoute -> Maybe Model -> ( Model, Cmd Msg )
init appState questionnaireUuid projectRoute mbOriginalModel =
    let
        ( pageModel, pageCmd ) =
            initPageModel appState questionnaireUuid projectRoute

        loadQuestionnaire =
            ( Loading
            , QuestionnairesApi.getQuestionnaire questionnaireUuid appState GetQuestionnaireComplete
            )

        ( questionnaire, questionnaireCmd ) =
            case mbOriginalModel of
                Just originalModel ->
                    case originalModel.questionnaire of
                        Success q ->
                            if q.uuid == questionnaireUuid then
                                ( Success q, Cmd.none )

                            else
                                loadQuestionnaire

                        _ ->
                            loadQuestionnaire

                _ ->
                    loadQuestionnaire
    in
    ( { questionnaire = questionnaire
      , pageModel = pageModel
      }
    , Cmd.batch
        [ questionnaireCmd
        , pageCmd
        ]
    )


initPageModel : AppState -> String -> ProjectRoute -> ( PageModel, Cmd Msg )
initPageModel appState questionnaireUuid projectRoute =
    let
        map toModel toMsg ( subModel, subCmd ) =
            ( toModel subModel, Cmd.map toMsg subCmd )
    in
    case projectRoute of
        ProjectRoute.Overview ->
            Overview.init appState questionnaireUuid
                |> map Overview OverviewMsg

        ProjectRoute.Planning ->
            Planning.init appState questionnaireUuid
                |> map Planning PlanningMsg

        ProjectRoute.Starred ->
            Starred.init appState questionnaireUuid
                |> map Starred StarredMsg

        ProjectRoute.Metrics ->
            Metrics.init appState questionnaireUuid
                |> map Metrics MetricsMsg

        ProjectRoute.Documents ->
            Documents.init appState questionnaireUuid
                |> map Documents DocumentsMsg

        ProjectRoute.Settings ->
            Settings.init appState questionnaireUuid
                |> map Settings SettingsMsg



-- UPDATE


type Msg
    = GetQuestionnaireComplete (Result ApiError Questionnaire)
    | OverviewMsg Overview.Msg
    | PlanningMsg Planning.Msg
    | StarredMsg Starred.Msg
    | MetricsMsg Metrics.Msg
    | DocumentsMsg Documents.Msg
    | SettingsMsg Settings.Msg


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case ( msg, model.pageModel ) of
        ( GetQuestionnaireComplete result, _ ) ->
            case result of
                Ok questionnaire ->
                    ( { model | questionnaire = Success questionnaire }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | questionnaire = ApiError.toActionResult "Unable to get project" error }
                      -- TODO maybe logout
                    , Cmd.none
                    )

        ( OverviewMsg overviewMsg, Overview overviewModel ) ->
            Overview.update appState overviewMsg overviewModel
                |> updateWith Overview OverviewMsg model

        ( PlanningMsg planningMsg, Planning planningModel ) ->
            Planning.update appState planningMsg planningModel
                |> updateWith Planning PlanningMsg model

        ( StarredMsg starredMsg, Starred starredModel ) ->
            Starred.update appState starredMsg starredModel
                |> updateWith Starred StarredMsg model

        ( MetricsMsg metricsMsg, Metrics metricsModel ) ->
            Metrics.update appState metricsMsg metricsModel
                |> updateWith Metrics MetricsMsg model

        ( DocumentsMsg documentsMsg, Documents documentsModel ) ->
            Documents.update appState documentsMsg documentsModel
                |> updateWith Documents DocumentsMsg model

        ( SettingsMsg settingsMsg, Settings settingsModel ) ->
            Settings.update appState settingsMsg settingsModel
                |> updateWith Settings SettingsMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith :
    (subModel -> PageModel)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | pageModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : AppState -> Model -> { title : String, content : Html Msg }
view appState model =
    { title = ActionResult.unwrap "Project" .name model.questionnaire
    , content = ActionResultWrapper.page appState.theme (viewContent appState model) model.questionnaire
    }


viewContent : AppState -> Model -> Questionnaire -> Html Msg
viewContent appState model questionnaire =
    let
        activePage =
            case model.pageModel of
                Overview _ ->
                    ProjectMenu.Overview

                Planning _ ->
                    ProjectMenu.Planning

                Starred _ ->
                    ProjectMenu.Starred

                Metrics _ ->
                    ProjectMenu.Metrics

                Documents _ ->
                    ProjectMenu.Documents

                Settings _ ->
                    ProjectMenu.Settings
    in
    SideNavigation.wrapper [ Animation.fadeIn, Animation.fast ]
        (ProjectMenu.view appState questionnaire activePage)
        (viewProjectPage appState model)


viewProjectPage : AppState -> Model -> Html Msg
viewProjectPage appState model =
    case model.pageModel of
        Overview overviewModel ->
            Html.map OverviewMsg <|
                Overview.view appState overviewModel

        Planning planningModel ->
            Html.map PlanningMsg <|
                Planning.view appState planningModel

        Starred starredModel ->
            Html.map StarredMsg <|
                Starred.view appState starredModel

        Metrics metricsModel ->
            Html.map MetricsMsg <|
                Metrics.view appState metricsModel

        Documents documentsModel ->
            Html.map DocumentsMsg <|
                Documents.view appState documentsModel

        Settings settingsModel ->
            Html.map SettingsMsg <|
                Settings.view appState settingsModel
