module View exposing (view)

import Browser exposing (Document)
import Common.AppState exposing (AppState)
import Common.Locale exposing (l)
import Common.View.Layout as Layout
import Common.View.Page as Page
import Dashboard.View
import Html exposing (..)
import KMEditor.View
import KnowledgeModels.View
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Organization.View
import Public.View
import Questionnaires.View
import Routes
import Users.View


l_ : String -> AppState -> String
l_ =
    l "View"


view : Model -> Document Msg
view model =
    if not model.appState.valid then
        Layout.misconfigured model.appState

    else
        case model.appState.route of
            Routes.DashboardRoute ->
                Dashboard.View.view model.appState model.dashboardModel
                    |> Layout.app model

            Routes.QuestionnairesRoute route ->
                model.questionnairesModel
                    |> Questionnaires.View.view route model.appState
                    |> Html.map QuestionnairesMsg
                    |> Layout.app model

            Routes.KMEditorRoute route ->
                model.kmEditorModel
                    |> KMEditor.View.view route model.appState
                    |> Html.map KMEditorMsg
                    |> Layout.app model

            Routes.KnowledgeModelsRoute route ->
                model.kmPackagesModel
                    |> KnowledgeModels.View.view route model.appState
                    |> Html.map KnowledgeModelsMsg
                    |> Layout.app model

            Routes.OrganizationRoute ->
                model.organizationModel
                    |> Organization.View.view model.appState
                    |> Html.map OrganizationMsg
                    |> Layout.app model

            Routes.PublicRoute route ->
                model.publicModel
                    |> Public.View.view route model.appState
                    |> Html.map PublicMsg
                    |> Layout.public model

            Routes.UsersRoute route ->
                model.users
                    |> Users.View.view route model.appState
                    |> Html.map UsersMsg
                    |> Layout.app model

            Routes.NotAllowedRoute ->
                notAllowedView model.appState
                    |> Layout.app model

            Routes.NotFoundRoute ->
                if model.appState.session.user == Nothing then
                    Layout.public model <| notFoundView model.appState

                else
                    Layout.app model <| notFoundView model.appState


notFoundView : AppState -> Html msg
notFoundView appState =
    Page.illustratedMessage
        { image = "page_not_found"
        , heading = l_ "notFound.title" appState
        , lines = [ l_ "notFound.message" appState ]
        }


notAllowedView : AppState -> Html msg
notAllowedView appState =
    Page.illustratedMessage
        { image = "security"
        , heading = l_ "notAllowed.title" appState
        , lines = [ l_ "notAllowed.message" appState ]
        }
