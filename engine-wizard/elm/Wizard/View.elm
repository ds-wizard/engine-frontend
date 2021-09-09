module Wizard.View exposing (view)

import Browser exposing (Document)
import Html exposing (Html)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Layout as Layout
import Wizard.Common.View.Page as Page
import Wizard.Dashboard.View
import Wizard.Documents.View
import Wizard.KMEditor.View
import Wizard.KnowledgeModels.View
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))
import Wizard.Projects.View
import Wizard.Public.View
import Wizard.Registry.View
import Wizard.Routes as Routes
import Wizard.Settings.View
import Wizard.Templates.View
import Wizard.Users.View


l_ : String -> AppState -> String
l_ =
    l "Wizard.View"


view : Model -> Document Msg
view model =
    if not model.appState.valid then
        Layout.misconfigured model.appState

    else
        case model.appState.route of
            Routes.DashboardRoute ->
                Wizard.Dashboard.View.view model.appState model.dashboardModel
                    |> Html.map DashboardMsg
                    |> Layout.app model

            Routes.DocumentsRoute route ->
                model.documentsModel
                    |> Wizard.Documents.View.view route model.appState
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

            Routes.ProjectsRoute route ->
                model.plansModel
                    |> Wizard.Projects.View.view route model.appState
                    |> Html.map PlansMsg
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
        { image = "page_not_found"
        , heading = l_ "notFound.title" appState
        , lines = [ l_ "notFound.message" appState ]
        , cy = "not-found"
        }


notAllowedView : AppState -> Html msg
notAllowedView appState =
    Page.illustratedMessage
        { image = "security"
        , heading = l_ "notAllowed.title" appState
        , lines = [ l_ "notAllowed.message" appState ]
        , cy = "not-allowed"
        }
