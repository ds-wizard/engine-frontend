module Update exposing (..)

import Auth.Update
import DSPlanner.Update
import KMEditor.Create.Update
import KMEditor.Editor.Update
import KMEditor.Index.Update
import KMEditor.Migration.Update
import KMEditor.Publish.Update
import KMPackages.Detail.Update
import KMPackages.Import.Update
import KMPackages.Index.Update
import Models exposing (Model, initLocalModel)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Organization.Update
import Public.Update
import Routing exposing (Route(..), isAllowed, parseLocation)
import Users.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.route of
        Organization ->
            Organization.Update.getCurrentOrganizationCmd model.session

        KMPackages ->
            KMPackages.Index.Update.getPackagesCmd model.session

        KMPackagesDetail organizationId kmId ->
            KMPackages.Detail.Update.getPackagesFilteredCmd organizationId kmId model.session

        KMEditor ->
            KMEditor.Index.Update.getKnowledgeModelsCmd model.session

        KMEditorCreate ->
            KMEditor.Create.Update.getPackagesCmd model.session

        KMEditorPublish uuid ->
            KMEditor.Publish.Update.getKnowledgeModelCmd uuid model.session

        KMEditorEditor uuid ->
            KMEditor.Editor.Update.getKnowledgeModelCmd uuid model.session

        KMEditorMigration uuid ->
            KMEditor.Migration.Update.getMigrationCmd uuid model.session

        Public route ->
            Public.Update.fetchData route Msgs.PublicMsg

        DSPlanner route ->
            DSPlanner.Update.fetchData route Msgs.QuestionnairesMsg model.session

        Users route ->
            Users.Update.fetchData route Msgs.UserManagementMsg model.session

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

        Msgs.OrganizationMsg msg ->
            let
                ( organizationModel, cmd ) =
                    Organization.Update.update msg model.session model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        Msgs.PackageManagementIndexMsg msg ->
            let
                ( kmPackagesIndexModel, cmd ) =
                    KMPackages.Index.Update.update msg model.session model.kmPackagesIndexModel
            in
            ( { model | kmPackagesIndexModel = kmPackagesIndexModel }, cmd )

        Msgs.PackageManagementDetailMsg msg ->
            let
                ( kmPackagesDetailModel, cmd ) =
                    KMPackages.Detail.Update.update msg model.session model.kmPackagesDetailModel
            in
            ( { model | kmPackagesDetailModel = kmPackagesDetailModel }, cmd )

        Msgs.PackageManagementImportMsg msg ->
            let
                ( kmPackagesImportModel, cmd ) =
                    KMPackages.Import.Update.update msg model.session model.kmPackagesImportModel
            in
            ( { model | kmPackagesImportModel = kmPackagesImportModel }, cmd )

        Msgs.KnowledgeModelsIndexMsg msg ->
            let
                ( kmEditorIndexModel, cmd ) =
                    KMEditor.Index.Update.update msg model.session model.kmEditorIndexModel
            in
            ( { model | kmEditorIndexModel = kmEditorIndexModel }, cmd )

        Msgs.KnowledgeModelsCreateMsg msg ->
            let
                ( seed, kmEditorCreateModel, cmd ) =
                    KMEditor.Create.Update.update msg model.seed model.session model.kmEditorCreateModel
            in
            ( { model | seed = seed, kmEditorCreateModel = kmEditorCreateModel }, cmd )

        Msgs.KnowledgeModelsPublishMsg msg ->
            let
                ( kmEditorPublishModel, cmd ) =
                    KMEditor.Publish.Update.update msg model.session model.kmEditorPublishModel
            in
            ( { model | kmEditorPublishModel = kmEditorPublishModel }, cmd )

        Msgs.KnowledgeModelsEditorMsg msg ->
            let
                ( seed, kmEditorEditorModel, cmd ) =
                    KMEditor.Editor.Update.update msg model.seed model.session model.kmEditorEditorModel
            in
            ( { model | seed = seed, kmEditorEditorModel = kmEditorEditorModel }, cmd )

        Msgs.KnowledgeModelsMigrationMsg msg ->
            let
                ( kmEditorMigrationModel, cmd ) =
                    KMEditor.Migration.Update.update msg model.session model.kmEditorMigrationModel
            in
            ( { model | kmEditorMigrationModel = kmEditorMigrationModel }, cmd )

        Msgs.PublicMsg msg ->
            let
                ( seed, publicModel, cmd ) =
                    Public.Update.update msg Msgs.PublicMsg model.seed model.publicModel
            in
            ( { model | seed = seed, publicModel = publicModel }, cmd )

        Msgs.QuestionnairesMsg msg ->
            let
                ( dsPlannerModel, cmd ) =
                    DSPlanner.Update.update msg Msgs.QuestionnairesMsg model.session model.dsPlannerModel
            in
            ( { model | dsPlannerModel = dsPlannerModel }, cmd )

        Msgs.UserManagementMsg msg ->
            let
                ( seed, users, cmd ) =
                    Users.Update.update msg Msgs.UserManagementMsg model.seed model.session model.users
            in
            ( { model | seed = seed, users = users }, cmd )
