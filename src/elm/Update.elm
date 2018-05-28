module Update exposing (..)

import Auth.Models exposing (setSidebarCollapsed)
import Auth.Update
import DSPlanner.Update
import KMEditor.Create.Update
import KMEditor.Editor.Update
import KMEditor.Index.Update
import KMEditor.Migration.Update
import KMEditor.Publish.Update
import KMPackages.Update
import Models exposing (Model, initLocalModel)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Organization.Update
import Ports
import Public.Update
import Routing exposing (Route(..), isAllowed, parseLocation)
import Users.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.route of
        DSPlanner route ->
            DSPlanner.Update.fetchData route Msgs.DSPlannerMsg model.session

        KMEditorCreate ->
            KMEditor.Create.Update.getPackagesCmd model.session

        KMEditorEditor uuid ->
            KMEditor.Editor.Update.getKnowledgeModelCmd uuid model.session

        KMEditorIndex ->
            KMEditor.Index.Update.getKnowledgeModelsCmd model.session

        KMEditorMigration uuid ->
            KMEditor.Migration.Update.getMigrationCmd uuid model.session

        KMEditorPublish uuid ->
            KMEditor.Publish.Update.getKnowledgeModelCmd uuid model.session

        KMPackages route ->
            KMPackages.Update.fetchData route Msgs.KMPackagesMsg model.session

        Organization ->
            Organization.Update.getCurrentOrganizationCmd model.session

        Public route ->
            Public.Update.fetchData route Msgs.PublicMsg

        Users route ->
            Users.Update.fetchData route Msgs.UsersMsg model.session

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

        Msgs.SetSidebarCollapsed collapsed ->
            let
                newModel =
                    { model | session = setSidebarCollapsed model.session collapsed }
            in
            ( newModel, Ports.storeSession <| Just newModel.session )

        Msgs.DSPlannerMsg msg ->
            let
                ( dsPlannerModel, cmd ) =
                    DSPlanner.Update.update msg Msgs.DSPlannerMsg model.session model.dsPlannerModel
            in
            ( { model | dsPlannerModel = dsPlannerModel }, cmd )

        Msgs.KMEditorCreateMsg msg ->
            let
                ( seed, kmEditorCreateModel, cmd ) =
                    KMEditor.Create.Update.update msg model.seed model.session model.kmEditorCreateModel
            in
            ( { model | seed = seed, kmEditorCreateModel = kmEditorCreateModel }, cmd )

        Msgs.KMEditorEditorMsg msg ->
            let
                ( seed, kmEditorEditorModel, cmd ) =
                    KMEditor.Editor.Update.update msg model.seed model.session model.kmEditorEditorModel
            in
            ( { model | seed = seed, kmEditorEditorModel = kmEditorEditorModel }, cmd )

        Msgs.KMEditorIndexMsg msg ->
            let
                ( kmEditorIndexModel, cmd ) =
                    KMEditor.Index.Update.update msg model.session model.kmEditorIndexModel
            in
            ( { model | kmEditorIndexModel = kmEditorIndexModel }, cmd )

        Msgs.KMEditorMigrationMsg msg ->
            let
                ( kmEditorMigrationModel, cmd ) =
                    KMEditor.Migration.Update.update msg model.session model.kmEditorMigrationModel
            in
            ( { model | kmEditorMigrationModel = kmEditorMigrationModel }, cmd )

        Msgs.KMEditorPublishMsg msg ->
            let
                ( kmEditorPublishModel, cmd ) =
                    KMEditor.Publish.Update.update msg model.session model.kmEditorPublishModel
            in
            ( { model | kmEditorPublishModel = kmEditorPublishModel }, cmd )

        Msgs.KMPackagesMsg msg ->
            let
                ( kmPackagesModel, cmd ) =
                    KMPackages.Update.update msg Msgs.KMPackagesMsg model.session model.kmPackagesModel
            in
            ( { model | kmPackagesModel = kmPackagesModel }, cmd )

        Msgs.OrganizationMsg msg ->
            let
                ( organizationModel, cmd ) =
                    Organization.Update.update msg model.session model.organizationModel
            in
            ( { model | organizationModel = organizationModel }, cmd )

        Msgs.PublicMsg msg ->
            let
                ( seed, publicModel, cmd ) =
                    Public.Update.update msg Msgs.PublicMsg model.seed model.publicModel
            in
            ( { model | seed = seed, publicModel = publicModel }, cmd )

        Msgs.UsersMsg msg ->
            let
                ( seed, users, cmd ) =
                    Users.Update.update msg Msgs.UsersMsg model.seed model.session model.users
            in
            ( { model | seed = seed, users = users }, cmd )
