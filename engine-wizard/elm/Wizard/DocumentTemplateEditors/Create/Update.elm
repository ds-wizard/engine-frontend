module Wizard.DocumentTemplateEditors.Create.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Form exposing (Form)
import Form.Field as Field
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Utils.RequestHelpers as RequestHelpers
import String.Normalize as Normalize
import Version exposing (Version)
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.DocumentTemplateEditors.Common.DocumentTemplateEditorCreateForm as DocumentTemplateEditorCreateForm exposing (DocumentTemplateEditorCreateForm)
import Wizard.DocumentTemplateEditors.Create.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Create.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case ( model.selectedDocumentTemplate, model.edit ) of
        ( Just documentTemplateId, True ) ->
            DocumentTemplatesApi.getTemplate appState documentTemplateId GetDocumentTemplateCompleted

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Cancel ->
            ( model, Ports.historyBack (Routing.toUrl appState Routes.documentTemplateEditorsIndex) )

        FormMsg formMsg ->
            handleFormMsg wrapMsg formMsg appState model

        FormSetVersion version ->
            handleFormSetVersion appState version model

        PostDocumentTemplateDraftCompleted result ->
            handlePostDocumentTemplateDraftCompleted appState model result

        DocumentTemplateTypeHintInputMsg typeHintInputMsg ->
            handleDocumentTemplateTypeHintInputMsg wrapMsg typeHintInputMsg appState model

        GetDocumentTemplateCompleted result ->
            case result of
                Ok documentTemplate ->
                    let
                        nextVersion =
                            Version.nextMinor documentTemplate.version

                        form =
                            model.form
                                |> setDocumentTemplateEditorCreateFormValue appState "name" documentTemplate.name
                                |> setDocumentTemplateEditorCreateFormValue appState "templateId" documentTemplate.templateId
                                |> setDocumentTemplateEditorCreateFormValue appState "versionMajor" (String.fromInt (Version.getMajor nextVersion))
                                |> setDocumentTemplateEditorCreateFormValue appState "versionMinor" (String.fromInt (Version.getMinor nextVersion))
                                |> setDocumentTemplateEditorCreateFormValue appState "versionPatch" (String.fromInt (Version.getPatch nextVersion))
                    in
                    ( { model | documentTemplate = ActionResult.Success documentTemplate, form = form }, Cmd.none )

                Err error ->
                    ( { model | documentTemplate = ApiError.toActionResult appState (gettext "Unable to get the document template." appState.locale) error }, Cmd.none )


handleFormMsg : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormMsg wrapMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just documentTemplateCreateForm ) ->
            let
                body =
                    DocumentTemplateEditorCreateForm.encode documentTemplateCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        DocumentTemplateDraftsApi.postDraft appState body PostDocumentTemplateDraftCompleted
            in
            ( { model | savingDocumentTemplate = ActionResult.Loading }, cmd )

        _ ->
            let
                newForm =
                    Form.update (DocumentTemplateEditorCreateForm.validation appState) formMsg model.form

                templateIdEmpty =
                    Maybe.unwrap True String.isEmpty (Form.getFieldAsString "templateId" model.form).value

                formWithTemplateId =
                    case ( formMsg, templateIdEmpty ) of
                        ( Form.Blur "name", True ) ->
                            let
                                suggestedTemplateId =
                                    (Form.getFieldAsString "name" model.form).value
                                        |> Maybe.unwrap "" Normalize.slug
                            in
                            setDocumentTemplateEditorCreateFormValue appState "templateId" suggestedTemplateId newForm

                        _ ->
                            newForm
            in
            ( { model | form = formWithTemplateId }, Cmd.none )


handleFormSetVersion : AppState -> Version -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleFormSetVersion appState version model =
    let
        form =
            model.form
                |> setDocumentTemplateEditorCreateFormValue appState "versionMajor" (String.fromInt (Version.getMajor version))
                |> setDocumentTemplateEditorCreateFormValue appState "versionMinor" (String.fromInt (Version.getMinor version))
                |> setDocumentTemplateEditorCreateFormValue appState "versionPatch" (String.fromInt (Version.getPatch version))
    in
    ( { model | form = form }, Cmd.none )


handlePostDocumentTemplateDraftCompleted : AppState -> Model -> Result ApiError DocumentTemplateDraftDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostDocumentTemplateDraftCompleted appState model result =
    case result of
        Ok documentTemplate ->
            ( model
            , cmdNavigate appState (Routes.documentTemplateEditorDetail documentTemplate.id)
            )

        Err error ->
            ( { model
                | form = Form.setFormErrors appState error model.form
                , savingDocumentTemplate = ApiError.toActionResult appState (gettext "Document template could not be created." appState.locale) error
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleDocumentTemplateTypeHintInputMsg : (Msg -> Wizard.Msgs.Msg) -> TypeHintInput.Msg DocumentTemplateSuggestion -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDocumentTemplateTypeHintInputMsg wrapMsg typeHintInputMsg appState model =
    let
        formMsg =
            wrapMsg << FormMsg << Form.Input "basedOn" Form.Select << Field.String

        cfg =
            { wrapMsg = wrapMsg << DocumentTemplateTypeHintInputMsg
            , getTypeHints = DocumentTemplatesApi.getTemplatesSuggestions appState (Just False) True
            , getError = gettext "Unable to get Knowledge Models." appState.locale
            , setReply = formMsg << .id
            , clearReply = Just <| formMsg ""
            , filterResults = Nothing
            }

        ( packageTypeHintInputModel, cmd ) =
            TypeHintInput.update cfg typeHintInputMsg model.documentTemplateTypeHintInputModel
    in
    ( { model | documentTemplateTypeHintInputModel = packageTypeHintInputModel }, cmd )


setDocumentTemplateEditorCreateFormValue : AppState -> String -> String -> Form FormError DocumentTemplateEditorCreateForm -> Form FormError DocumentTemplateEditorCreateForm
setDocumentTemplateEditorCreateFormValue appState field value =
    Form.update (DocumentTemplateEditorCreateForm.validation appState) (Form.Input field Form.Text (Field.String value))
