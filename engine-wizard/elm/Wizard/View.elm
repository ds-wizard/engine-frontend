module Wizard.View exposing (view)

import Browser exposing (Document)
import Gettext exposing (gettext)
import Html exposing (Html)
import Shared.Undraw as Undraw
import Wizard.Apps.View
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Layout as Layout
import Wizard.Common.View.Page as Page
import Wizard.Dashboard.View
import Wizard.Dev.View
import Wizard.Documents.View
import Wizard.KMEditor.View
import Wizard.KnowledgeModels.View
import Wizard.Locales.View
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.ProjectImporters.View
import Wizard.Projects.View
import Wizard.Public.View
import Wizard.Registry.View
import Wizard.Routes as Routes
import Wizard.Settings.View
import Wizard.Templates.View
import Wizard.Users.View


view : Model -> Document Msg
view model =
    if not model.appState.valid then
        Layout.misconfigured model.appState

    else
        case model.appState.route of
            Routes.DevRoute route ->
                Wizard.Dev.View.view route model.appState model.adminModel
                    |> Html.map AdminMsg
                    |> Layout.app model

            Routes.AppsRoute route ->
                Wizard.Apps.View.view route model.appState model.appsModel
                    |> Html.map AppsMsg
                    |> Layout.app model

            Routes.DashboardRoute ->
                Wizard.Dashboard.View.view model.appState model.dashboardModel
                    |> Html.map DashboardMsg
                    |> Layout.app model

            Routes.DocumentsRoute _ ->
                model.documentsModel
                    |> Wizard.Documents.View.view model.appState
                    |> Html.map DocumentsMsg
                    |> Layout.app model

            Routes.KMEditorRoute route ->
                model.kmEditorModel
                    |> Wizard.KMEditor.View.view route model.appState
                    |> Html.map KMEditorMsg
                    |> Layout.app model

            Routes.KnowledgeModelsRoute route ->
                model.kmPackagesModel
                    |> Wizard.KnowledgeModels.View.view route model.appState
                    |> Html.map KnowledgeModelsMsg
                    |> Layout.mixedApp model

            Routes.LocalesRoute route ->
                model.localeModel
                    |> Wizard.Locales.View.view route model.appState
                    |> Html.map LocaleMsg
                    |> Layout.app model

            Routes.ProjectImportersRoute _ ->
                model.projectImportersModel
                    |> Wizard.ProjectImporters.View.view model.appState
                    |> Html.map ProjectImportersMsg
                    |> Layout.app model

            Routes.ProjectsRoute route ->
                model.projectsModel
                    |> Wizard.Projects.View.view route model.appState
                    |> Html.map ProjectsMsg
                    |> Layout.mixedApp model

            Routes.PublicRoute route ->
                model.publicModel
                    |> Wizard.Public.View.view route model.appState
                    |> Html.map PublicMsg
                    |> Layout.public model

            Routes.RegistryRoute route ->
                model.registryModel
                    |> Wizard.Registry.View.view route model.appState
                    |> Html.map RegistryMsg
                    |> Layout.app model

            Routes.SettingsRoute route ->
                model.settingsModel
                    |> Wizard.Settings.View.view route model.appState
                    |> Html.map SettingsMsg
                    |> Layout.app model

            Routes.TemplatesRoute route ->
                model.templatesModel
                    |> Wizard.Templates.View.view route model.appState
                    |> Html.map TemplatesMsg
                    |> Layout.app model

            Routes.UsersRoute route ->
                model.users
                    |> Wizard.Users.View.view route model.appState
                    |> Html.map UsersMsg
                    |> Layout.app model

            Routes.NotAllowedRoute ->
                notAllowedView model.appState
                    |> Layout.app model

            Routes.NotFoundRoute ->
                Layout.mixedApp model <| notFoundView model.appState


notFoundView : AppState -> Html msg
notFoundView appState =
    Page.illustratedMessage
        { image = Undraw.pageNotFound
        , heading = gettext "Not Found" appState.locale
        , lines = [ gettext "The page you are looking for was not found." appState.locale ]
        , cy = "not-found"
        }


notAllowedView : AppState -> Html msg
notAllowedView appState =
    Page.illustratedMessage
        { image = Undraw.security
        , heading = gettext "Not Allowed" appState.locale
        , lines = [ gettext "You don't have a permission to view this page." appState.locale ]
        , cy = "not-allowed"
        }
