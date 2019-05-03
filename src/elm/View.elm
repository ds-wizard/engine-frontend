module View exposing (view)

import Browser exposing (Document)
import Common.Html exposing (emptyNode)
import Common.Html.Attribute exposing (detailClass)
import Common.View.Layout as Layout
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import KMEditor.View
import KnowledgeModels.View
import Markdown
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Organization.View
import Public.View
import Questionnaires.View
import Routing exposing (Route(..))
import Users.View


view : Model -> Document Msg
view model =
    case model.appState.route of
        Welcome ->
            welcomeView model
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
            Layout.app model notFoundView


welcomeView : Model -> Html Msg
welcomeView model =
    let
        warning =
            case model.appState.welcome.warning of
                Just message ->
                    div [ class "alert alert-warning" ]
                        [ Markdown.toHtml [] message ]

                Nothing ->
                    emptyNode

        info =
            case model.appState.welcome.info of
                Just message ->
                    div [ class "alert alert-info" ]
                        [ Markdown.toHtml [] message ]

                Nothing ->
                    emptyNode
    in
    div [ detailClass "Welcome" ]
        [ warning
        , info
        , Page.message "hand-spock-o" <| "Welcome to the " ++ model.appState.appTitle ++ "!"
        ]


notFoundView : Html msg
notFoundView =
    Page.message "file-o" "The page was not found"


notAllowedView : Html msg
notAllowedView =
    Page.message "ban" "You don't have a permission to view this page"
