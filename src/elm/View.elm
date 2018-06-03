module View exposing (view)

import Common.Html exposing (detailContainerClass, linkTo)
import Common.View exposing (defaultFullPageError, fullPageMessage, pageHeader)
import Common.View.Layout exposing (appView, publicView)
import DSPlanner.View
import Html exposing (..)
import KMEditor.View
import KMPackages.View
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Organization.View
import Public.View
import Routing exposing (Route(..), homeRoute, loginRoute, signupRoute)
import Users.View


view : Model -> Html Msg
view model =
    case model.route of
        Welcome ->
            appView model welcomeView

        DSPlanner route ->
            model.dsPlannerModel
                |> DSPlanner.View.view route DSPlannerMsg
                |> appView model

        KMEditor route ->
            model.kmEditorModel
                |> KMEditor.View.view route KMEditorMsg model.jwt
                |> appView model

        KMPackages route ->
            model.kmPackagesModel
                |> KMPackages.View.view route KMPackagesMsg
                |> appView model

        Organization ->
            model.organizationModel
                |> Organization.View.view
                |> appView model

        Public route ->
            model.publicModel
                |> Public.View.view route PublicMsg
                |> publicView

        Users route ->
            model.users
                |> Users.View.view route UsersMsg
                |> appView model

        NotAllowed ->
            appView model notAllowedView

        NotFound ->
            appView model notFoundView

        DataManagementPlans ->
            appView model dataManagementPlansView


welcomeView : Html Msg
welcomeView =
    fullPageMessage "fa-hand-spock-o" "Welcome to the Data Stewardship Wizard!"


dataManagementPlansView : Html Msg
dataManagementPlansView =
    div [ detailContainerClass ]
        [ pageHeader "Data Management Plans" []
        , fullPageMessage "fa-book" "Data Management Plans are not implemented yet."
        ]


notFoundView : Html msg
notFoundView =
    fullPageMessage "fa-file-o" "The page was not found"


notAllowedView : Html msg
notAllowedView =
    fullPageMessage "fa-ban" "You don't have a permission to view this page"
