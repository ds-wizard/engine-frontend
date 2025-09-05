module Wizard.View exposing (view)

import Browser exposing (Document)
import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Layouts.Layout as Layout
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Pages.Comments.View
import Wizard.Pages.Dashboard.View
import Wizard.Pages.Dev.View
import Wizard.Pages.DocumentTemplateEditors.View
import Wizard.Pages.DocumentTemplates.View
import Wizard.Pages.Documents.View
import Wizard.Pages.KMEditor.View
import Wizard.Pages.KnowledgeModelSecrets.View
import Wizard.Pages.KnowledgeModels.View
import Wizard.Pages.Locales.View
import Wizard.Pages.ProjectActions.View
import Wizard.Pages.ProjectFiles.View
import Wizard.Pages.ProjectImporters.View
import Wizard.Pages.Projects.View
import Wizard.Pages.Public.View
import Wizard.Pages.Registry.View
import Wizard.Pages.Settings.View
import Wizard.Pages.Tenants.View
import Wizard.Pages.Users.View
import Wizard.Routes as Routes


view : Model -> Document Msg
view model =
    if not model.appState.valid then
        Layout.misconfigured model.appState

    else
        case model.appState.route of
            Routes.DevRoute route ->
                Wizard.Pages.Dev.View.view route model.appState model.adminModel
                    |> Html.map AdminMsg
                    |> Layout.app model

            Routes.TenantsRoute route ->
                Wizard.Pages.Tenants.View.view route model.appState model.tenantsModel
                    |> Html.map TenantsMsg
                    |> Layout.app model

            Routes.CommentsRoute _ _ ->
                Wizard.Pages.Comments.View.view model.appState model.commentsModel
                    |> Html.map CommentsMsg
                    |> Layout.app model

            Routes.DashboardRoute ->
                Wizard.Pages.Dashboard.View.view model.appState model.dashboardModel
                    |> Html.map DashboardMsg
                    |> Layout.app model

            Routes.DocumentsRoute _ ->
                model.documentsModel
                    |> Wizard.Pages.Documents.View.view model.appState
                    |> Html.map DocumentsMsg
                    |> Layout.app model

            Routes.DocumentTemplateEditorsRoute route ->
                model.documentTemplateEditorsModel
                    |> Wizard.Pages.DocumentTemplateEditors.View.view route model.appState
                    |> Html.map DocumentTemplateEditorsMsg
                    |> Layout.app model

            Routes.DocumentTemplatesRoute route ->
                model.documentTemplatesModel
                    |> Wizard.Pages.DocumentTemplates.View.view route model.appState
                    |> Html.map DocumentTemplatesMsg
                    |> Layout.app model

            Routes.KMEditorRoute route ->
                model.kmEditorModel
                    |> Wizard.Pages.KMEditor.View.view route model.appState
                    |> Html.map KMEditorMsg
                    |> Layout.app model

            Routes.KnowledgeModelsRoute route ->
                model.kmPackagesModel
                    |> Wizard.Pages.KnowledgeModels.View.view route model.appState
                    |> Html.map KnowledgeModelsMsg
                    |> Layout.mixedApp model

            Routes.KnowledgeModelSecretsRoute ->
                model.kmSecretsModel
                    |> Wizard.Pages.KnowledgeModelSecrets.View.view model.appState
                    |> Html.map KnowledgeModelSecretsMsg
                    |> Layout.app model

            Routes.LocalesRoute route ->
                model.localeModel
                    |> Wizard.Pages.Locales.View.view route model.appState
                    |> Html.map LocaleMsg
                    |> Layout.app model

            Routes.ProjectActionsRoute _ ->
                model.projectActionsModel
                    |> Wizard.Pages.ProjectActions.View.view model.appState
                    |> Html.map ProjectActionsMsg
                    |> Layout.app model

            Routes.ProjectFilesRoute _ ->
                model.projectFilesModel
                    |> Wizard.Pages.ProjectFiles.View.view model.appState
                    |> Html.map ProjectFilesMsg
                    |> Layout.app model

            Routes.ProjectImportersRoute _ ->
                model.projectImportersModel
                    |> Wizard.Pages.ProjectImporters.View.view model.appState
                    |> Html.map ProjectImportersMsg
                    |> Layout.app model

            Routes.ProjectsRoute route ->
                model.projectsModel
                    |> Wizard.Pages.Projects.View.view route model.appState
                    |> Html.map ProjectsMsg
                    |> Layout.mixedApp model

            Routes.PublicRoute route ->
                model.publicModel
                    |> Wizard.Pages.Public.View.view route model.appState
                    |> Html.map PublicMsg
                    |> Layout.public model

            Routes.RegistryRoute route ->
                model.registryModel
                    |> Wizard.Pages.Registry.View.view route model.appState
                    |> Html.map RegistryMsg
                    |> Layout.app model

            Routes.SettingsRoute route ->
                model.settingsModel
                    |> Wizard.Pages.Settings.View.view route model.appState
                    |> Html.map SettingsMsg
                    |> Layout.app model

            Routes.UsersRoute route ->
                model.users
                    |> Wizard.Pages.Users.View.view route model.appState
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
        , lines = [ gettext "You don't have permission to view this page." appState.locale ]
        , cy = "not-allowed"
        }
