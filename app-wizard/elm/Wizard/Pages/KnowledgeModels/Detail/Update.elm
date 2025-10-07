module Wizard.Pages.KnowledgeModels.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setPackage)
import Gettext exposing (gettext)
import Wizard.Api.Models.Package.PackagePhase exposing (PackagePhase)
import Wizard.Api.Packages as PackagesApi
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData packageId appState =
    PackagesApi.getPackage appState packageId GetPackageCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setPackage
                , defaultError = gettext "Unable to get the Knowledge Model." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , locale = appState.locale
                }

        DropdownMsg state ->
            ( { model | dropdownState = state }, Cmd.none )

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            deleteVersionCompleted appState model result

        UpdatePhase phase ->
            handleSetUpdatePhase wrapMsg appState model phase

        UpdatePhaseCompleted phase result ->
            case model.package of
                Success package ->
                    RequestHelpers.applyResultTransform
                        { setResult = setPackage
                        , defaultError = gettext "Unable to update the knowledge model." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        , transform = always { package | phase = phase }
                        , locale = appState.locale
                        }

                _ ->
                    ( model, Cmd.none )

        ExportPackage package ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (PackagesApi.exportPackageUrl package.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowAllVersions ->
            ( { model | showAllVersions = True }, Cmd.none )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.package of
        Success package ->
            ( { model | deletingVersion = Loading }
            , PackagesApi.deletePackageVersion appState package.id (wrapMsg << DeleteVersionCompleted)
            )

        _ ->
            ( model, Cmd.none )


deleteVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteVersionCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.knowledgeModelsIndex )

        Err error ->
            ( { model | deletingVersion = ApiError.toActionResult appState (gettext "Knowledge Model could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleSetUpdatePhase : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> PackagePhase -> ( Model, Cmd Wizard.Msgs.Msg )
handleSetUpdatePhase wrapMsg appState model phase =
    case model.package of
        Success package ->
            let
                newPackage =
                    { package | phase = phase }
            in
            ( model, PackagesApi.putPackage appState newPackage (wrapMsg << UpdatePhaseCompleted phase) )

        _ ->
            ( model, Cmd.none )
