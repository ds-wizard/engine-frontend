module Wizard.DocumentTemplateEditors.Editor.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Gettext exposing (gettext)
import Random exposing (Seed)
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplateDraftDetail as DocumentTemplateDraftDetail
import Shared.Error.ApiError as ApiError
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.TemplateEditor as TemplateEditor
import Wizard.DocumentTemplateEditors.Editor.Models exposing (CurrentEditor(..), Model)
import Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : String -> AppState -> Cmd Msg
fetchData documentTemplateId appState =
    Cmd.batch
        [ DocumentTemplateDraftsApi.getDraft documentTemplateId appState GetTemplateCompleted
        , Cmd.map FileEditorMsg (FileEditor.fetchData documentTemplateId appState)
        ]


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
            if TemplateEditor.formChanged m.templateEditorModel && not (FileEditor.anyFileSaving m.fileEditorModel) then
                let
                    ( newSeed, templateEditorModel, templateEditorCmd ) =
                        TemplateEditor.update templateEditorUpdateConfig appState TemplateEditor.saveMsg model.templateEditorModel
                in
                ( newSeed
                , { model | templateEditorModel = templateEditorModel }
                , Cmd.batch [ cmd, templateEditorCmd ]
                )

            else
                wrap model

        templateEditorUpdateConfig : TemplateEditor.UpdateConfig Wizard.Msgs.Msg
        templateEditorUpdateConfig =
            { wrapMsg = wrapMsg << TemplateEditorMsg
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

        previewUpdateConfig : Preview.UpdateConfig Wizard.Msgs.Msg
        previewUpdateConfig =
            { wrapMsg = wrapMsg << PreviewMsg
            , logoutMsg = Wizard.Msgs.logoutMsg
            , documentTemplateId = model.documentTemplateId
            , documentTemplate = model.documentTemplate
            , updatePreviewSettings = wrapMsg << UpdatePreviewSettings
            }
    in
    case msg of
        GetTemplateCompleted result ->
            case result of
                Ok documentTemplate ->
                    withSeed <|
                        ( { model
                            | documentTemplate = ActionResult.Success documentTemplate
                            , templateEditorModel = TemplateEditor.setDocumentTemplate documentTemplate model.templateEditorModel
                            , previewModel = Preview.setSelectedQuestionnaire documentTemplate.questionnaire model.previewModel
                          }
                        , Cmd.none
                        )

                Err error ->
                    withSeed <|
                        ( { model | documentTemplate = ApiError.toActionResult appState (gettext "Unable to get document template" appState.locale) error }
                        , getResultCmd Wizard.Msgs.logoutMsg result
                        )

        SetEditor editor ->
            let
                newModel =
                    { model | currentEditor = editor }
            in
            if editor == PreviewEditor then
                let
                    ( previewModel, previewCmd ) =
                        Preview.update previewUpdateConfig appState Preview.loadPreviewMsg model.previewModel
                in
                withSeed ( { newModel | previewModel = previewModel }, previewCmd )

            else
                wrap newModel

        TemplateEditorMsg templateEditorMsg ->
            let
                ( newSeed, templateEditorModel, templateEditorCmd ) =
                    TemplateEditor.update templateEditorUpdateConfig appState templateEditorMsg model.templateEditorModel
            in
            ( newSeed, { model | templateEditorModel = templateEditorModel }, templateEditorCmd )

        FileEditorMsg fileEditorMsg ->
            let
                ( fileEditorModel, fileEditorCmd ) =
                    FileEditor.update fileEditorUpdateConfig appState fileEditorMsg model.fileEditorModel
            in
            withSeed ( { model | fileEditorModel = fileEditorModel }, fileEditorCmd )

        PreviewMsg previewMsg ->
            let
                ( previewModel, previewCmd ) =
                    Preview.update previewUpdateConfig appState previewMsg model.previewModel
            in
            withSeed ( { model | previewModel = previewModel }, previewCmd )

        PublishModalMsg publishModalMsg ->
            let
                publishModalUpdateConfig =
                    { wrapMsg = wrapMsg << PublishModalMsg
                    , documentTemplateId = model.documentTemplateId
                    , documentTemplateForm = TemplateEditor.getFormOutput model.templateEditorModel
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
