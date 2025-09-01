module Wizard.DocumentTemplateEditors.Editor.Update exposing
    ( fetchData
    , isGuarded
    , update
    )

import ActionResult
import Gettext exposing (gettext)
import Random exposing (Seed)
import Shared.Data.ApiError as ApiError
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.Models.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail
import Wizard.Api.Prefabs as PrefabsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.Settings as Settings
import Wizard.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute exposing (DTEditorRoute)
import Wizard.DocumentTemplateEditors.Editor.Models exposing (CurrentEditor(..), Model, containsChanges)
import Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes


fetchData : AppState -> String -> DTEditorRoute -> Model -> Cmd Msg
fetchData appState documentTemplateId subroute model =
    if ActionResult.unwrap False ((==) documentTemplateId << .id) model.documentTemplate then
        loadPreviewCmd (subroute == DTEditorRoute.Preview)

    else
        Cmd.batch
            [ DocumentTemplateDraftsApi.getDraft appState documentTemplateId GetTemplateCompleted
            , PrefabsApi.getDocumentTemplateFormatPrefabs appState GetDocumentTemplateFormatPrefabsCompleted
            , PrefabsApi.getDocumentTemplateFormatStepPrefabs appState GetDocumentTemplateFormatStepPrefabsCompleted
            , Cmd.map FileEditorMsg (FileEditor.fetchData documentTemplateId appState)
            , loadPreviewCmd (subroute == DTEditorRoute.Preview)
            ]


loadPreviewCmd : Bool -> Cmd Msg
loadPreviewCmd isPreview =
    if isPreview then
        Task.dispatch (PreviewMsg Preview.loadPreviewMsg)

    else
        Cmd.none


isGuarded : AppState -> Routes.Route -> Model -> Maybe String
isGuarded appState nextRoute model =
    if not (containsChanges model) then
        Nothing

    else if Routes.isDocumentTemplateEditor model.documentTemplateId nextRoute then
        Nothing

    else
        Just (gettext "There are unsaved changes." appState.locale)


update : AppState -> (Msg -> Wizard.Msgs.Msg) -> Msg -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update appState wrapMsg msg model =
    let
        wrap : Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
        wrap m =
            ( appState.seed, m, Cmd.none )

        withSeed : ( Model, Cmd Wizard.Msgs.Msg ) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
        withSeed ( m, cmd ) =
            ( appState.seed, m, cmd )

        saveForm : ( Model, Cmd Wizard.Msgs.Msg ) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
        saveForm ( m, cmd ) =
            if Settings.formChanged m.settingsModel && not (FileEditor.anyFileSaving m.fileEditorModel) then
                let
                    ( newSeed, templateEditorModel, templateEditorCmd ) =
                        Settings.update templateEditorUpdateConfig appState Settings.saveMsg model.settingsModel
                in
                ( newSeed
                , { model | settingsModel = templateEditorModel }
                , Cmd.batch [ cmd, templateEditorCmd ]
                )

            else
                wrap model

        updateUnloadMessage : ( Seed, Model, Cmd Wizard.Msgs.Msg ) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
        updateUnloadMessage ( seed, m, cmd ) =
            if containsChanges m && not m.unloadMessageSet then
                ( seed
                , { m | unloadMessageSet = True }
                , Cmd.batch
                    [ cmd
                    , Ports.setUnloadMessage (gettext "There are unsaved changes." appState.locale)
                    ]
                )

            else if not (containsChanges m) && m.unloadMessageSet then
                ( seed
                , { m | unloadMessageSet = True }
                , Cmd.batch
                    [ cmd
                    , Ports.clearUnloadMessage ()
                    ]
                )

            else
                ( seed, m, cmd )

        templateEditorUpdateConfig : Settings.UpdateConfig Wizard.Msgs.Msg
        templateEditorUpdateConfig =
            { wrapMsg = wrapMsg << SettingsMsg
            , logoutMsg = Wizard.Msgs.logoutMsg
            , documentTemplateId = model.documentTemplateId
            , updateDocumentTemplate = wrapMsg << UpdateDocumentTemplate
            }

        fileEditorUpdateConfig : FileEditor.UpdateConfig Wizard.Msgs.Msg
        fileEditorUpdateConfig =
            { wrapMsg = wrapMsg << FileEditorMsg
            , logoutMsg = Wizard.Msgs.logoutMsg
            , documentTemplateId = model.documentTemplateId
            , onFileSavedMsg = wrapMsg SaveForm
            }
    in
    case msg of
        GetTemplateCompleted result ->
            case result of
                Ok documentTemplate ->
                    withSeed <|
                        ( { model
                            | documentTemplate = ActionResult.Success documentTemplate
                            , settingsModel = Settings.setDocumentTemplate appState documentTemplate model.settingsModel
                            , previewModel =
                                model.previewModel
                                    |> Preview.setSelectedQuestionnaire documentTemplate.questionnaire
                                    |> Preview.setSelectedBranch documentTemplate.branch
                          }
                        , Cmd.none
                        )

                Err error ->
                    withSeed <|
                        ( { model | documentTemplate = ApiError.toActionResult appState (gettext "Unable to get document template" appState.locale) error }
                        , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                        )

        GetDocumentTemplateFormatPrefabsCompleted result ->
            case result of
                Ok prefabs ->
                    withSeed ( { model | documentTemplateFormatPrefabs = ActionResult.Success <| List.map .content prefabs }, Cmd.none )

                Err _ ->
                    withSeed ( { model | documentTemplateFormatPrefabs = ActionResult.Error "" }, Cmd.none )

        GetDocumentTemplateFormatStepPrefabsCompleted result ->
            case result of
                Ok prefabs ->
                    withSeed ( { model | documentTemplateFormatStepPrefabs = ActionResult.Success <| List.map .content prefabs }, Cmd.none )

                Err _ ->
                    withSeed ( { model | documentTemplateFormatStepPrefabs = ActionResult.Error "" }, Cmd.none )

        SettingsMsg settingsMsg ->
            let
                ( newSeed, settingsModel, settingsCmd ) =
                    Settings.update templateEditorUpdateConfig appState settingsMsg model.settingsModel
            in
            updateUnloadMessage <|
                ( newSeed, { model | settingsModel = settingsModel }, settingsCmd )

        FileEditorMsg fileEditorMsg ->
            let
                ( fileEditorModel, fileEditorCmd ) =
                    FileEditor.update fileEditorUpdateConfig appState fileEditorMsg model.fileEditorModel
            in
            updateUnloadMessage <|
                withSeed ( { model | fileEditorModel = fileEditorModel }, fileEditorCmd )

        PreviewMsg previewMsg ->
            let
                previewUpdateConfig : Preview.UpdateConfig Wizard.Msgs.Msg
                previewUpdateConfig =
                    { wrapMsg = wrapMsg << PreviewMsg
                    , logoutMsg = Wizard.Msgs.logoutMsg
                    , documentTemplateId = model.documentTemplateId
                    , documentTemplate = model.documentTemplate
                    , updatePreviewSettings = wrapMsg << UpdatePreviewSettings
                    }

                ( previewModel, previewCmd ) =
                    Preview.update previewUpdateConfig appState previewMsg model.previewModel
            in
            withSeed ( { model | previewModel = previewModel }, previewCmd )

        PublishModalMsg publishModalMsg ->
            let
                publishModalUpdateConfig =
                    { wrapMsg = wrapMsg << PublishModalMsg
                    , documentTemplateId = model.documentTemplateId
                    , documentTemplateForm = Settings.getFormOutput model.settingsModel
                    }

                ( publishModalModel, publishModalCmd ) =
                    PublishModal.update publishModalUpdateConfig appState publishModalMsg model.publishModalModel
            in
            withSeed ( { model | publishModalModel = publishModalModel }, publishModalCmd )

        UpdatePreviewSettings previewSettings ->
            wrap { model | documentTemplate = ActionResult.map (DocumentTemplateDraftDetail.updatePreviewSettings previewSettings) model.documentTemplate }

        UpdateDocumentTemplate documentTemplate ->
            wrap { model | documentTemplate = ActionResult.Success documentTemplate }

        Save ->
            if FileEditor.filesChanged model.fileEditorModel then
                let
                    ( fileEditorModel, fileEditorCmd ) =
                        FileEditor.update fileEditorUpdateConfig appState FileEditor.saveMsg model.fileEditorModel
                in
                withSeed ( { model | fileEditorModel = fileEditorModel }, fileEditorCmd )

            else
                saveForm ( model, Cmd.none )

        SaveForm ->
            saveForm ( model, Cmd.none )

        DiscardChanges ->
            withSeed
                ( { model
                    | fileEditorModel = FileEditor.initialModel
                    , settingsModel = Settings.initialModel appState
                  }
                , Cmd.batch
                    [ DocumentTemplateDraftsApi.getDraft appState model.documentTemplateId (wrapMsg << GetTemplateCompleted)
                    , Cmd.map (wrapMsg << FileEditorMsg) (FileEditor.fetchData model.documentTemplateId appState)
                    , Cmd.map wrapMsg (loadPreviewCmd (model.currentEditor == PreviewEditor))
                    , Ports.clearUnloadMessage ()
                    ]
                )
