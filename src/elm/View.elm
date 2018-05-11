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
import KMPackages.Detail.View
import KMPackages.Import.View
import KMPackages.Index.View
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

        Organization ->
            model.organizationModel
                |> Organization.View.view
                |> appView model

        KMEditorCreate ->
            model.kmEditorCreateModel
                |> KMEditor.Create.View.view
                |> appView model

        KMEditorEditor uuid ->
            model.kmEditorEditorModel
                |> KMEditor.Editor.View.view
                |> appView model

        KMEditor ->
            model.kmEditorIndexModel
                |> KMEditor.Index.View.view model.jwt
                |> appView model

        KMEditorPublish uuid ->
            model.kmEditorPublishModel
                |> KMEditor.Publish.View.view
                |> appView model

        KMEditorMigration uuid ->
            model.kmEditorMigrationModel
                |> KMEditor.Migration.View.view
                |> appView model

        KMPackages ->
            model.kmPackagesIndexModel
                |> KMPackages.Index.View.view
                |> appView model

        KMPackagesDetail organizationId kmId ->
            model.kmPackagesDetailModel
                |> KMPackages.Detail.View.view
                |> appView model

        KMPackagesImport ->
            model.kmPackagesImportModel
                |> KMPackages.Import.View.view
                |> appView model

        DSPlanner route ->
            model.dsPlannerModel
                |> DSPlanner.View.view route QuestionnairesMsg
                |> appView model

        DataManagementPlans ->
            appView model dataManagementPlansView

        Public route ->
            model.publicModel
                |> Public.View.view route PublicMsg
                |> publicView

        Users route ->
            model.users
                |> Users.View.view route UserManagementMsg
                |> appView model

        NotFound ->
            appView model notFoundView

        NotAllowed ->
            appView model notAllowedView


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
