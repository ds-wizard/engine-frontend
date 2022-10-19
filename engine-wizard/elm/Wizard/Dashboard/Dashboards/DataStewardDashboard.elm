module Wizard.Dashboard.Dashboards.DataStewardDashboard exposing (Model, Msg, fetchData, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Api.Packages as PackagesApi
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Package exposing (Package)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Dashboard.Widgets.CreateKnowledgeModelWidget as CreateKnowledgeModelWidget
import Wizard.Dashboard.Widgets.CreateProjectTemplateWidget as CreateProjectTemplateWidget
import Wizard.Dashboard.Widgets.ImportDocumentTemplateWidget as ImportDocumentTemplateWidget
import Wizard.Dashboard.Widgets.ImportKnowledgeModelWidget as ImportKnowledgeModelWidget
import Wizard.Dashboard.Widgets.OutdatedPackagesWidget as OutdatedPackagesWidget
import Wizard.Dashboard.Widgets.OutdatedTemplatesWidget as OutdatedTemplatesWidget
import Wizard.Dashboard.Widgets.WelcomeWidget as WelcomeWidget


type alias Model =
    { packages : ActionResult (List Package)
    , templates : ActionResult (List Template)
    }


initialModel : Model
initialModel =
    { packages = ActionResult.Loading
    , templates = ActionResult.Loading
    }


type Msg
    = GetPackagesComplete (Result ApiError (Pagination Package))
    | GetTemplatesComplete (Result ApiError (Pagination Template))


fetchData : AppState -> Cmd Msg
fetchData appState =
    let
        packagesCmd =
            PackagesApi.getOutdatedPackages appState GetPackagesComplete

        templatesCmd =
            TemplatesApi.getOutdatedTemplates appState GetTemplatesComplete
    in
    Cmd.batch [ packagesCmd, templatesCmd ]


update : Msg -> AppState -> Model -> Model
update msg appState model =
    case msg of
        GetPackagesComplete result ->
            case result of
                Ok data ->
                    { model | packages = ActionResult.Success data.items }

                Err error ->
                    { model | packages = ApiError.toActionResult appState (lg "apiError.packages.getListError" appState) error }

        GetTemplatesComplete result ->
            case result of
                Ok data ->
                    { model | templates = ActionResult.Success data.items }

                Err error ->
                    { model | templates = ApiError.toActionResult appState (lg "apiError.templates.getListError" appState) error }


view : AppState -> Model -> Html msg
view appState model =
    div []
        [ div [ class "row gx-3" ]
            [ WelcomeWidget.view appState
            , OutdatedPackagesWidget.view appState model.packages
            , OutdatedTemplatesWidget.view appState model.templates
            , CreateKnowledgeModelWidget.view appState
            , CreateProjectTemplateWidget.view appState
            , ImportKnowledgeModelWidget.view appState
            , ImportDocumentTemplateWidget.view appState
            ]
        ]
