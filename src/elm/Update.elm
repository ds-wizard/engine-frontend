module Update exposing (..)

import Auth.Update
import KnowledgeModels.Index.Models
import KnowledgeModels.Index.Update
import Models exposing (Model)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Organization.Models
import Organization.Update
import PackageManagement.Detail.Models
import PackageManagement.Detail.Update
import PackageManagement.Import.Models
import PackageManagement.Import.Update
import PackageManagement.Index.Models
import PackageManagement.Index.Update
import Routing exposing (Route(..), isAllowed, parseLocation)
import UserManagement.Create.Models
import UserManagement.Create.Update
import UserManagement.Delete.Update
import UserManagement.Edit.Models
import UserManagement.Edit.Update
import UserManagement.Index.Models
import UserManagement.Index.Update


fetchData : Model -> Cmd Msg
fetchData model =
    case model.route of
        UserManagement ->
            UserManagement.Index.Update.getUsersCmd model.session

        UserManagementEdit uuid ->
            UserManagement.Edit.Update.getUserCmd uuid model.session

        UserManagementDelete uuid ->
            UserManagement.Delete.Update.getUserCmd uuid model.session

        Organization ->
            Organization.Update.getCurrentOrganizationCmd model.session

        PackageManagement ->
            PackageManagement.Index.Update.getPackagesCmd model.session

        PackageManagementDetail pkgName ->
            PackageManagement.Detail.Update.getPackageCmd pkgName model.session

        KnowledgeModels ->
            KnowledgeModels.Index.Update.getKnowledgeModelsCmd model.session

        _ ->
            Cmd.none


initLocalModel : Model -> Model
initLocalModel model =
    case model.route of
        UserManagement ->
            { model | userManagementIndexModel = UserManagement.Index.Models.initialModel }

        UserManagementCreate ->
            { model | userManagementCreateModel = UserManagement.Create.Models.initialModel }

        UserManagementEdit uuid ->
            { model | userManagementEditModel = UserManagement.Edit.Models.initialModel uuid }

        Organization ->
            { model | organizationModel = Organization.Models.initialModel }

        PackageManagement ->
            { model | packageManagementIndexModel = PackageManagement.Index.Models.initialModel }

        PackageManagementDetail pkgName ->
            { model | packageManagementDetailModel = PackageManagement.Detail.Models.initialModel }

        PackageManagementImport ->
            { model | packageManagementImportModel = PackageManagement.Import.Models.initialModel }

        KnowledgeModels ->
            { model | knowledgeModelsIndexModel = KnowledgeModels.Index.Models.initialModel }

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
                ( seed, userManagementCreateModel, cmd ) =
                    UserManagement.Create.Update.update msg model.seed model.session model.userManagementCreateModel
            in
            ( { model | seed = seed, userManagementCreateModel = userManagementCreateModel }, cmd )

        Msgs.UserManagementDeleteMsg msg ->
            let
                ( userManagementDeleteModel, cmd ) =
                    UserManagement.Delete.Update.update msg model.session model.userManagementDeleteModel
            in
            ( { model | userManagementDeleteModel = userManagementDeleteModel }, cmd )

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
