module Update exposing (..)

import Auth.Models exposing (setSidebarCollapsed)
import Auth.Update
import DSPlanner.Update
import KMEditor.Update
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

        KMEditor route ->
            KMEditor.Update.fetchData route Msgs.KMEditorMsg model.session

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

        Msgs.KMEditorMsg msg ->
            let
                ( seed, kmEditorModel, cmd ) =
                    KMEditor.Update.update msg Msgs.KMEditorMsg model.seed model.session model.kmEditorModel
            in
            ( { model | seed = seed, kmEditorModel = kmEditorModel }, cmd )

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
