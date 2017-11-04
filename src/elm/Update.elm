module Update exposing (..)

import Auth.Update
import Models exposing (Model)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Routing exposing (Route(..), isAllowed, parseLocation)
import UserManagement.Create.Models
import UserManagement.Create.Update
import UserManagement.Delete.Update
import UserManagement.Index.Models
import UserManagement.Index.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.route of
        UserManagement ->
            UserManagement.Index.Update.listUsersCmd model.session

        UserManagementDelete uuid ->
            UserManagement.Delete.Update.getUserCmd uuid model.session

        _ ->
            Cmd.none


initLocalModel : Model -> Model
initLocalModel model =
    case model.route of
        UserManagement ->
            { model | userManagementIndexModel = UserManagement.Index.Models.initialModel }

        UserManagementCreate ->
            { model | userManagementCreateModel = UserManagement.Create.Models.initialModel 0 }

        _ ->
            model


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
                ( userManagementCreateModel, cmd ) =
                    UserManagement.Create.Update.update msg model.session model.userManagementCreateModel
            in
            ( { model | userManagementCreateModel = userManagementCreateModel }, cmd )

        Msgs.UserManagementDeleteMsg msg ->
            let
                ( userManagementDeleteModel, cmd ) =
                    UserManagement.Delete.Update.update msg model.session model.userManagementDeleteModel
            in
            ( { model | userManagementDeleteModel = userManagementDeleteModel }, cmd )
