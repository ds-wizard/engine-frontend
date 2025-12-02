module Wizard.Pages.KnowledgeModels.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelPackage)
import Gettext exposing (gettext)
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData kmPackageId appState =
    KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmPackageId GetKnowledgeModelPackageCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetKnowledgeModelPackageCompleted result ->
            RequestHelpers.applyResult
                { setResult = setKnowledgeModelPackage
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
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    RequestHelpers.applyResultTransform
                        { setResult = setKnowledgeModelPackage
                        , defaultError = gettext "Unable to update the knowledge model." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        , transform = always { kmPackage | phase = phase }
                        , locale = appState.locale
                        }

                _ ->
                    ( model, Cmd.none )

        ExportKnowledgeModelPackage kmPackage ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (KnowledgeModelPackagesApi.exportKnowledgeModelPackageUrl kmPackage.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowAllVersions ->
            ( { model | showAllVersions = True }, Cmd.none )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.knowledgeModelPackage of
        Success kmPackage ->
            ( { model | deletingVersion = Loading }
            , KnowledgeModelPackagesApi.deleteKnowledgeModelPackageVersion appState kmPackage.id (wrapMsg << DeleteVersionCompleted)
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


handleSetUpdatePhase : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> KnowledgeModelPackagePhase -> ( Model, Cmd Wizard.Msgs.Msg )
handleSetUpdatePhase wrapMsg appState model phase =
    case model.knowledgeModelPackage of
        Success kmPackage ->
            let
                newPackage =
                    { kmPackage | phase = phase }
            in
            ( model, KnowledgeModelPackagesApi.putKnowledgeModelPackage appState newPackage (wrapMsg << UpdatePhaseCompleted phase) )

        _ ->
            ( model, Cmd.none )
