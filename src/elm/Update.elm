module Update exposing (..)

import Auth.Update
import KnowledgeModels.Create.Update
import KnowledgeModels.Editor.Update
import KnowledgeModels.Index.Update
import KnowledgeModels.Migration.Update
import KnowledgeModels.Publish.Update
import Models exposing (Model, initLocalModel)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Organization.Update
import PackageManagement.Detail.Update
import PackageManagement.Import.Update
import PackageManagement.Index.Update
import Public.Update
import Routing exposing (Route(..), isAllowed, parseLocation)
import UserManagement.Create.Update
import UserManagement.Edit.Update
import UserManagement.Index.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.route of
        UserManagement ->
            UserManagement.Index.Update.getUsersCmd model.session

        UserManagementEdit uuid ->
            UserManagement.Edit.Update.getUserCmd uuid model.session

        Organization ->
            Organization.Update.getCurrentOrganizationCmd model.session

        PackageManagement ->
            PackageManagement.Index.Update.getPackagesCmd model.session

        PackageManagementDetail groupId artifactId ->
            PackageManagement.Detail.Update.getPackagesFilteredCmd groupId artifactId model.session

        KnowledgeModels ->
            KnowledgeModels.Index.Update.getKnowledgeModelsCmd model.session

        KnowledgeModelsCreate ->
            KnowledgeModels.Create.Update.getPackagesCmd model.session

        KnowledgeModelsPublish uuid ->
            KnowledgeModels.Publish.Update.getKnowledgeModelCmd uuid model.session

        KnowledgeModelsEditor uuid ->
            KnowledgeModels.Editor.Update.getKnowledgeModelCmd uuid model.session

        KnowledgeModelsMigration uuid ->
            KnowledgeModels.Migration.Update.getMigrationCmd uuid model.session

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.ChangeLocation path ->
            ( model, Navigation.newUrl path )

        Msgs.OnLocationChange location ->
            let
                newModel =
                    { model | route = parseLocation location }
            in
            ( initLocalModel newModel, fetchData newModel )

        Msgs.AuthMsg msg ->
            Auth.Update.update msg model

        Msgs.UserManagementIndexMsg msg ->
            let
                ( userManagementIndexModel, cmd ) =
                    UserManagement.Index.Update.update msg model.session model.userManagementIndexModel
            in
            ( { model | userManagementIndexModel = userManagementIndexModel }, cmd )

        Msgs.UserManagementCreateMsg msg ->
            let
                ( seed, userManagementCreateModel, cmd ) =
                    UserManagement.Create.Update.update msg model.seed model.session model.userManagementCreateModel
            in
            ( { model | seed = seed, userManagementCreateModel = userManagementCreateModel }, cmd )

        Msgs.UserManagementEditMsg msg ->
            let
                ( userManagementEditModel, cmd ) =
                    UserManagement.Edit.Update.update msg model.session model.userManagementEditModel
            in
            ( { model | userManagementEditModel = userManagementEditModel }, cmd )

        Msgs.OrganizationMsg msg ->
            let
                ( organizationModel, cmd ) =
                    Organization.Update.update msg model.session model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        Msgs.PackageManagementIndexMsg msg ->
            let
                ( packageManagementIndexModel, cmd ) =
                    PackageManagement.Index.Update.update msg model.session model.packageManagementIndexModel
            in
            ( { model | packageManagementIndexModel = packageManagementIndexModel }, cmd )

        Msgs.PackageManagementDetailMsg msg ->
            let
                ( packageManagementDetailModel, cmd ) =
                    PackageManagement.Detail.Update.update msg model.session model.packageManagementDetailModel
            in
            ( { model | packageManagementDetailModel = packageManagementDetailModel }, cmd )

        Msgs.PackageManagementImportMsg msg ->
            let
                ( packageManagementImportModel, cmd ) =
                    PackageManagement.Import.Update.update msg model.session model.packageManagementImportModel
            in
            ( { model | packageManagementImportModel = packageManagementImportModel }, cmd )

        Msgs.KnowledgeModelsIndexMsg msg ->
            let
                ( knowledgeModelsIndexModel, cmd ) =
                    KnowledgeModels.Index.Update.update msg model.session model.knowledgeModelsIndexModel
            in
            ( { model | knowledgeModelsIndexModel = knowledgeModelsIndexModel }, cmd )

        Msgs.KnowledgeModelsCreateMsg msg ->
            let
                ( seed, knowledgeModelsCreateModel, cmd ) =
                    KnowledgeModels.Create.Update.update msg model.seed model.session model.knowledgeModelsCreateModel
            in
            ( { model | seed = seed, knowledgeModelsCreateModel = knowledgeModelsCreateModel }, cmd )

        Msgs.KnowledgeModelsPublishMsg msg ->
            let
                ( knowledgeModelsPublishModel, cmd ) =
                    KnowledgeModels.Publish.Update.update msg model.session model.knowledgeModelsPublishModel
            in
            ( { model | knowledgeModelsPublishModel = knowledgeModelsPublishModel }, cmd )

        Msgs.KnowledgeModelsEditorMsg msg ->
            let
                ( seed, knowledgeModelsEditorModel, cmd ) =
                    KnowledgeModels.Editor.Update.update msg model.seed model.session model.knowledgeModelsEditorModel
            in
            ( { model | seed = seed, knowledgeModelsEditorModel = knowledgeModelsEditorModel }, cmd )

        Msgs.KnowledgeModelsMigrationMsg msg ->
            let
                ( knowledgeModelsMigrationModel, cmd ) =
                    KnowledgeModels.Migration.Update.update msg model.session model.knowledgeModelsMigrationModel
            in
            ( { model | knowledgeModelsMigrationModel = knowledgeModelsMigrationModel }, cmd )

        Msgs.PublicMsg msg ->
            let
                ( seed, publicModel, cmd ) =
                    Public.Update.update msg Msgs.PublicMsg model.seed model.publicModel
            in
            ( { model | seed = seed, publicModel = publicModel }, cmd )
