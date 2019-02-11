module View exposing (view)

import Browser exposing (Document)
import Common.Html exposing (detailContainerClassWith)
import Common.View.Layout exposing (appView, publicView)
import Common.View.Page as Page
import DSPlanner.View
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import KMEditor.View
import KMPackages.View
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Organization.View
import Public.View
import Routing exposing (Route(..))
import Users.View


view : Model -> Document Msg
view model =
    case model.state.route of
        Welcome ->
            appView model welcomeView

        DSPlanner route ->
            model.dsPlannerModel
                |> DSPlanner.View.view route DSPlannerMsg
                |> appView model

        KMEditor route ->
            model.kmEditorModel
                |> KMEditor.View.view route KMEditorMsg model.state.jwt
                |> appView model

        KMPackages route ->
            model.kmPackagesModel
                |> KMPackages.View.view route KMPackagesMsg model.state.jwt
                |> appView model

        Organization ->
            model.organizationModel
                |> Organization.View.view
                |> appView model

        Public route ->
            model.publicModel
                |> Public.View.view route PublicMsg
                |> publicView model

        Users route ->
            model.users
                |> Users.View.view route UsersMsg
                |> appView model

        NotAllowed ->
            appView model notAllowedView

        NotFound ->
            appView model notFoundView


welcomeView : Html Msg
welcomeView =
    div [ detailContainerClassWith "Welcome" ]
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
