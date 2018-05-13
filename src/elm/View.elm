module View exposing (view)

import Common.Html exposing (detailContainerClass, linkTo)
import Common.View exposing (defaultFullPageError, fullPageError, pageHeader)
import Common.View.Layout exposing (appView, publicView)
import DSPlanner.View
import Html exposing (..)
import KMEditor.Create.View
import KMEditor.Editor.View
import KMEditor.Index.View
import KMEditor.Migration.View
import KMEditor.Publish.View
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

        KMEditorCreate ->
            model.kmEditorCreateModel
                |> KMEditor.Create.View.view
                |> appView model

        KMEditorEditor uuid ->
            model.kmEditorEditorModel
                |> KMEditor.Editor.View.view
                |> appView model

        KMEditorIndex ->
            model.kmEditorIndexModel
                |> KMEditor.Index.View.view model.jwt
                |> appView model

        KMEditorMigration uuid ->
            model.kmEditorMigrationModel
                |> KMEditor.Migration.View.view
                |> appView model

        KMEditorPublish uuid ->
            model.kmEditorPublishModel
                |> KMEditor.Publish.View.view
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
    fullPageError "fa-hand-spock-o" "Welcome to the Data Stewardship Wizard!"


dataManagementPlansView : Html Msg
dataManagementPlansView =
    div [ detailContainerClass ]
        [ pageHeader "Data Management Plans" []
        , fullPageError "fa-book" "Data Management Plans are not implemented yet."
        ]


notFoundView : Html msg
notFoundView =
    fullPageError "fa-file-o" "The page was not found"


notAllowedView : Html msg
notAllowedView =
    fullPageError "fa-ban" "You don't have a permission to view this page"
