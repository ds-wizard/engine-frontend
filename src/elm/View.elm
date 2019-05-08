module View exposing (view)

import Browser exposing (Document)
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
import Routing exposing (Route(..))
import Users.View


view : Model -> Document Msg
view model =
    if not model.appState.valid then
        Layout.misconfigured

    else
        case model.appState.route of
            Dashboard ->
                Dashboard.View.view model.appState model.dashboardModel
                    |> Layout.app model

            Questionnaires route ->
                model.dsPlannerModel
                    |> Questionnaires.View.view route QuestionnairesMsg model.appState
                    |> Layout.app model

            KMEditor route ->
                model.kmEditorModel
                    |> KMEditor.View.view route KMEditorMsg model.appState
                    |> Layout.app model

            KnowledgeModels route ->
                model.kmPackagesModel
                    |> KnowledgeModels.View.view route KnowledgeModelsMsg model.appState
                    |> Layout.app model

            Organization ->
                model.organizationModel
                    |> Organization.View.view
                    |> Layout.app model

            Public route ->
                model.publicModel
                    |> Public.View.view route PublicMsg model.appState
                    |> Layout.public model

            Users route ->
                model.users
                    |> Users.View.view route UsersMsg
                    |> Layout.app model

            NotAllowed ->
                Layout.app model notAllowedView

            NotFound ->
                if model.appState.session.user == Nothing then
                    Layout.public model notFoundView

                else
                    Layout.app model notFoundView


notFoundView : Html msg
notFoundView =
    Page.illustratedMessage
        { image = "page_not_found"
        , heading = "Not Found"
        , lines = [ "The page you are looking for was not found." ]
        }


notAllowedView : Html msg
notAllowedView =
    Page.illustratedMessage
        { image = "security"
        , heading = "Not Allowed"
        , lines = [ "You don't have a permission to view this page." ]
        }
