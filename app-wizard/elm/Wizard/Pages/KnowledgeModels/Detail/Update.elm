module Wizard.Pages.KnowledgeModels.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setKnowledgeModelPackage)
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal
import Wizard.Pages.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Detail.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : Uuid -> AppState -> Cmd Msg
fetchData kmPackageUuid appState =
    KnowledgeModelPackagesApi.getKnowledgeModelPackage appState kmPackageUuid GetKnowledgeModelPackageCompleted


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

        DeleteModalMsg deleteModalMsg ->
            let
                deleteModalConfig =
                    { afterDeleteCmd = cmdNavigate appState Routes.knowledgeModelsIndex
                    , wrapMsg = wrapMsg << DeleteModalMsg
                    }

                ( deleteModalModel, deleteModalCmd ) =
                    DeleteModal.update appState deleteModalConfig deleteModalMsg model.deleteModalModel
            in
            ( { model | deleteModalModel = deleteModalModel }
            , deleteModalCmd
            )

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

        UpdatePublic isPublic ->
            handleSetUpdatePublic wrapMsg appState model isPublic

        UpdatePublicCompleted isPublic result ->
            case model.knowledgeModelPackage of
                Success kmPackage ->
                    RequestHelpers.applyResultTransform
                        { setResult = setKnowledgeModelPackage
                        , defaultError = gettext "Unable to update the knowledge model." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = Wizard.Msgs.logoutMsg
                        , transform = always { kmPackage | public = isPublic }
                        , locale = appState.locale
                        }

                _ ->
                    ( model, Cmd.none )

        ExportKnowledgeModelPackage kmPackage ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (KnowledgeModelPackagesApi.exportKnowledgeModelPackageUrl kmPackage.uuid)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowAllVersions ->
            ( { model | showAllVersions = True }, Cmd.none )


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


handleSetUpdatePublic : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Bool -> ( Model, Cmd Wizard.Msgs.Msg )
handleSetUpdatePublic wrapMsg appState model isPublic =
    case model.knowledgeModelPackage of
        Success kmPackage ->
            let
                newPackage =
                    { kmPackage | public = isPublic }
            in
            ( model, KnowledgeModelPackagesApi.putKnowledgeModelPackage appState newPackage (wrapMsg << UpdatePublicCompleted isPublic) )

        _ ->
            ( model, Cmd.none )
