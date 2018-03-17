module View exposing (view)

import Common.Html exposing (detailContainerClass, linkTo)
import Common.View exposing (defaultFullPageError, fullPageError, pageHeader)
import Common.View.Layout exposing (appView, publicView)
import Html exposing (..)
import KnowledgeModels.Create.View
import KnowledgeModels.Editor.View
import KnowledgeModels.Index.View
import KnowledgeModels.Migration.View
import KnowledgeModels.Publish.View
import Models exposing (Model)
import Msgs exposing (Msg(..))
import Organization.View
import PackageManagement.Detail.View
import PackageManagement.Import.View
import PackageManagement.Index.View
import Public.View
import Routing exposing (Route(..), homeRoute, loginRoute, signupRoute)
import UserManagement.View


view : Model -> Html Msg
view model =
    case model.route of
        Welcome ->
            appView model welcomeView

        Organization ->
            model.organizationModel
                |> Organization.View.view
                |> appView model

        KnowledgeModelsCreate ->
            model.knowledgeModelsCreateModel
                |> KnowledgeModels.Create.View.view
                |> appView model

        KnowledgeModelsEditor uuid ->
            model.knowledgeModelsEditorModel
                |> KnowledgeModels.Editor.View.view
                |> appView model

        KnowledgeModels ->
            model.knowledgeModelsIndexModel
                |> KnowledgeModels.Index.View.view model.jwt
                |> appView model

        KnowledgeModelsPublish uuid ->
            model.knowledgeModelsPublishModel
                |> KnowledgeModels.Publish.View.view
                |> appView model

        KnowledgeModelsMigration uuid ->
            model.knowledgeModelsMigrationModel
                |> KnowledgeModels.Migration.View.view
                |> appView model

        PackageManagement ->
            model.packageManagementIndexModel
                |> PackageManagement.Index.View.view
                |> appView model

        PackageManagementDetail groupId artifactId ->
            model.packageManagementDetailModel
                |> PackageManagement.Detail.View.view
                |> appView model

        PackageManagementImport ->
            model.packageManagementImportModel
                |> PackageManagement.Import.View.view
                |> appView model

        Wizards ->
            appView model wizzardsView

        DataManagementPlans ->
            appView model dataManagementPlansView

        Public route ->
            model.publicModel
                |> Public.View.view route PublicMsg
                |> publicView

        UserManagement route ->
            model.userManagement
                |> UserManagement.View.view route UserManagementMsg
                |> appView model

        NotFound ->
            appView model notFoundView

        NotAllowed ->
            appView model notAllowedView


welcomeView : Html Msg
welcomeView =
    fullPageError "fa-hand-spock-o" "Welcome to the Data Stewardship Wizard!"


wizzardsView : Html Msg
wizzardsView =
    div [ detailContainerClass ]
        [ pageHeader "Wizards" []
        , fullPageError "fa-magic" "Wizards are not implemented yet."
        ]


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
