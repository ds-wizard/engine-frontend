module View exposing (view)

import Browser exposing (Document)
import Common.Html.Attribute exposing (detailClass)
import Common.View.Layout as Layout
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
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
    case model.state.route of
        Welcome ->
            Layout.app model welcomeView

        Questionnaires route ->
            model.dsPlannerModel
                |> Questionnaires.View.view route QuestionnairesMsg
                |> Layout.app model

        KMEditor route ->
            model.kmEditorModel
                |> KMEditor.View.view route KMEditorMsg model.state.jwt
                |> Layout.app model

        KnowledgeModels route ->
            model.kmPackagesModel
                |> KnowledgeModels.View.view route KnowledgeModelsMsg model.state.jwt
                |> Layout.app model

        Organization ->
            model.organizationModel
                |> Organization.View.view
                |> Layout.app model

        Public route ->
            model.publicModel
                |> Public.View.view route PublicMsg
                |> Layout.public model

        Users route ->
            model.users
                |> Users.View.view route UsersMsg
                |> Layout.app model

        NotAllowed ->
            Layout.app model notAllowedView

        NotFound ->
            Layout.app model notFoundView


welcomeView : Html Msg
welcomeView =
    div [ detailClass "Welcome" ]
        [ div [ class "alert alert-warning" ]
            [ h4 [ class "alert-heading" ] [ text "Warning" ]
            , p [ class "mb-0" ] [ text "DSW is currently under intensive development. As such, we cannot guarantee DS plans compatibility in future versions." ]
            ]
        , div [ class "alert alert-info" ]
            [ p [ class "mb-0" ]
                [ text "This is a demonstration DSW installment. To deploy your local DSW copy, follow the instructions in "
                , a [ href "https://docs.ds-wizard.org", target "_blank" ] [ text "docs.ds-wizard.org" ]
                , text "."
                ]
            ]
        , Page.message "hand-spock-o" "Welcome to the Data Stewardship Wizard!"
        ]


notFoundView : Html msg
notFoundView =
    Page.message "file-o" "The page was not found"


notAllowedView : Html msg
notAllowedView =
    Page.message "ban" "You don't have a permission to view this page"
