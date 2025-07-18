module Wizard.DocumentTemplateEditors.Editor.Components.Settings exposing
    ( CurrentTemplateEditor
    , Model
    , Msg
    , UpdateConfig
    , ViewConfig
    , formChanged
    , getFormOutput
    , initialModel
    , saveMsg
    , setDocumentTemplate
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Form.Field as Field
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, label, span, strong, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Random exposing (Seed)
import Shared.Components.FontAwesome exposing (fa, faDelete)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form as Form
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (getUuid)
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Uuid
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Wizard.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.DocumentTemplateEditors.Editor.Components.TemplateEditor.DocumentTemplateForm as DocumentTemplateForm exposing (DocumentTemplateForm)
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)



-- MODEL


type alias Model =
    { currentEditor : CurrentTemplateEditor
    , form : Form FormError DocumentTemplateForm
    , formListsChanged : Bool
    , savingForm : ActionResult String
    }


type CurrentTemplateEditor
    = GeneralTemplateEditor
    | KnowledgeModelsTemplateEditor
    | FormatsTemplateEditor


initialModel : AppState -> Model
initialModel appState =
    { currentEditor = GeneralTemplateEditor
    , form = DocumentTemplateForm.initEmpty appState
    , formListsChanged = False
    , savingForm = ActionResult.Unset
    }


setDocumentTemplate : AppState -> DocumentTemplateDraftDetail -> Model -> Model
setDocumentTemplate appState detail model =
    { model | form = DocumentTemplateForm.init appState detail }


formChanged : Model -> Bool
formChanged model =
    Form.containsChanges model.form || model.formListsChanged


getFormOutput : Model -> Maybe DocumentTemplateForm
getFormOutput =
    Form.getOutput << .form



-- MSG


type Msg
    = FormMsg Form.Msg
    | SetTemplateEditor CurrentTemplateEditor
    | Save
    | PutTemplateCompleted (Result ApiError DocumentTemplateDraftDetail)
    | FillFormat Int DocumentTemplateFormatDraft
    | FillStep Int Int DocumentTemplateFormatStep


saveMsg : Msg
saveMsg =
    Save



-- UPDATE


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , documentTemplateId : String
    , updateDocumentTemplate : DocumentTemplateDraftDetail -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        wrap m =
            ( appState.seed, m, Cmd.none )

        withSeed ( m, cmd ) =
            ( appState.seed, m, cmd )
    in
    case msg of
        FormMsg formMsg ->
            let
                newForm =
                    Form.update (DocumentTemplateForm.validation appState) formMsg model.form

                ( newSeed, newFormWithUuid ) =
                    case ( formMsg, List.last (Form.getListIndexes "formats" newForm) ) of
                        ( Form.Append "formats", Just index ) ->
                            let
                                ( uuid, newSeed1 ) =
                                    getUuid appState.seed

                                uuidFormMsg =
                                    Form.Input ("formats." ++ String.fromInt index ++ ".uuid") Form.Text (Field.String (Uuid.toString uuid))
                            in
                            ( newSeed1
                            , Form.update (DocumentTemplateForm.validation appState) uuidFormMsg newForm
                            )

                        _ ->
                            ( appState.seed, newForm )

                formListsChanged =
                    case formMsg of
                        Form.Append _ ->
                            True

                        Form.RemoveItem _ _ ->
                            True

                        _ ->
                            model.formListsChanged
            in
            ( newSeed
            , { model
                | form = newFormWithUuid
                , formListsChanged = formListsChanged
              }
            , Cmd.none
            )

        SetTemplateEditor currentTemplateEditor ->
            wrap { model | currentEditor = currentTemplateEditor }

        Save ->
            case Form.getOutput model.form of
                Just documentTemplateForm ->
                    withSeed
                        ( { model | savingForm = ActionResult.Loading }
                        , DocumentTemplateDraftsApi.putDraft appState
                            cfg.documentTemplateId
                            (DocumentTemplateForm.encode DocumentTemplatePhase.Draft documentTemplateForm)
                            (cfg.wrapMsg << PutTemplateCompleted)
                        )

                Nothing ->
                    wrap { model | form = Form.update (DocumentTemplateForm.validation appState) Form.Submit model.form }

        PutTemplateCompleted result ->
            case result of
                Ok documentTemplate ->
                    if documentTemplate.id == cfg.documentTemplateId then
                        withSeed
                            ( { model
                                | savingForm = ActionResult.Success ""
                                , form = DocumentTemplateForm.init appState documentTemplate
                                , formListsChanged = False
                              }
                            , Task.dispatch (cfg.updateDocumentTemplate documentTemplate)
                            )

                    else
                        withSeed
                            ( model
                            , cmdNavigate appState (Routes.documentTemplateEditorDetail documentTemplate.id)
                            )

                Err error ->
                    withSeed
                        ( { model | savingForm = ApiError.toActionResult appState (gettext "Unable to save document template" appState.locale) error }
                        , RequestHelpers.getResultCmd cfg.logoutMsg result
                        )

        FillFormat i format ->
            wrap { model | form = DocumentTemplateForm.fillFormat appState i format model.form }

        FillStep formatIndex stepIndex step ->
            wrap { model | form = DocumentTemplateForm.fillStep appState formatIndex stepIndex step model.form }



-- VIEW


type alias ViewConfig =
    { documentTemplateFormatPrefabs : ActionResult (List DocumentTemplateFormatDraft)
    , documentTemplateFormatStepPrefabs : ActionResult (List DocumentTemplateFormatStep)
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    let
        navLink templateEditor linkLabel cy =
            a
                [ class "nav-link"
                , classList [ ( "active", model.currentEditor == templateEditor ) ]
                , onClick (SetTemplateEditor templateEditor)
                , dataCy cy
                ]
                [ text linkLabel ]

        content =
            case model.currentEditor of
                GeneralTemplateEditor ->
                    formViewGeneral appState model

                KnowledgeModelsTemplateEditor ->
                    formViewKnowledgeModel appState model

                FormatsTemplateEditor ->
                    formViewFormats appState cfg model
    in
    div [ class "DocumentTemplateEditor__MetadataEditor" ]
        [ div [ class "DocumentTemplateEditor__MetadataEditor__Navigation" ]
            [ div [ class "nav nav-pills flex-column" ]
                [ navLink GeneralTemplateEditor (gettext "General" appState.locale) "dt_template-nav_general"
                , navLink KnowledgeModelsTemplateEditor (gettext "Knowledge Models" appState.locale) "dt_template-nav_knowledge-models"
                , navLink FormatsTemplateEditor (gettext "Formats" appState.locale) "dt_template-nav_formats"
                ]
            ]
        , div [ class "DocumentTemplateEditor__MetadataEditor__Content" ] [ content ]
        ]



-- VIEW - General


formViewGeneral : AppState -> Model -> Html Msg
formViewGeneral appState model =
    let
        versionInputConfig =
            { label = gettext "Version" appState.locale
            , majorField = "versionMajor"
            , minorField = "versionMinor"
            , patchField = "versionPatch"
            , currentVersion = Nothing
            , wrapFormMsg = FormMsg
            , setVersionMsg = Nothing
            }
    in
    div []
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
        , Html.map FormMsg <| FormGroup.input appState model.form "description" <| gettext "Description" appState.locale
        , Html.map FormMsg <| FormGroup.input appState model.form "templateId" <| gettext "Document Template ID" appState.locale
        , FormExtra.textAfter <| gettext "Document template ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale
        , FormGroup.version appState versionInputConfig model.form
        , Html.map FormMsg <| FormGroup.input appState model.form "license" <| gettext "License" appState.locale
        , Html.map FormMsg <| FormGroup.markdownEditor appState model.form "readme" <| gettext "Readme" appState.locale
        ]



-- VIEW - Knowledge Models


formViewKnowledgeModel : AppState -> Model -> Html Msg
formViewKnowledgeModel appState model =
    let
        allowedInputHeader =
            div [ class "form-list-header mb-2" ]
                [ span [] [ text (gettext "Organization ID" appState.locale) ]
                , span [] [ text (gettext "Knowledge Model ID" appState.locale) ]
                , span [] [ text (gettext "Min Version" appState.locale) ]
                , span [] [ text (gettext "Max Version" appState.locale) ]
                ]
    in
    Html.map FormMsg <|
        div []
            [ FormGroup.listWithHeader appState allowedInputHeader allowedPackageFormView model.form "allowedPackages" (gettext "Allowed Knowledge Models" appState.locale) (gettext "Add knowledge model" appState.locale)
            ]


allowedPackageFormView : Form FormError DocumentTemplateForm -> Int -> Html Form.Msg
allowedPackageFormView form index =
    let
        fieldName name =
            "allowedPackages." ++ String.fromInt index ++ "." ++ name

        getField name =
            Form.getFieldAsString (fieldName name) form

        viewField name =
            Input.textInput (getField name) [ class "form-control", id (fieldName name) ]
    in
    div [ class "input-group mb-2" ]
        [ viewField "orgId"
        , viewField "kmId"
        , viewField "minVersion"
        , viewField "maxVersion"
        , button
            [ class "btn btn-link text-danger"
            , onClick (Form.RemoveItem "allowedPackages" index)
            ]
            [ faDelete ]
        ]



-- VIEW - Formats


formViewFormats : AppState -> ViewConfig -> Model -> Html Msg
formViewFormats appState cfg model =
    div []
        [ FormGroup.listWithCustomMsg appState FormMsg (formatFormView appState cfg) model.form "formats" (gettext "Formats" appState.locale) (gettext "Add format" appState.locale) ]


formatFormView : AppState -> ViewConfig -> Form FormError DocumentTemplateForm -> Int -> Html Msg
formatFormView appState cfg form index =
    let
        formatPrefabs =
            case ( DocumentTemplateForm.isFormatEmpty index form, cfg.documentTemplateFormatPrefabs ) of
                ( True, ActionResult.Success formats ) ->
                    let
                        viewFormat format =
                            a
                                [ onClick (FillFormat index format)
                                , class "btn btn-outline-primary me-1 with-icon"
                                ]
                                [ fa format.icon
                                , text format.name
                                ]
                    in
                    prefabsView appState (List.map viewFormat formats)

                _ ->
                    Html.nothing

        nameField =
            "formats." ++ String.fromInt index ++ ".name"

        iconField =
            "formats." ++ String.fromInt index ++ ".icon"

        stepsField =
            "formats." ++ String.fromInt index ++ ".steps"

        nameValue =
            Maybe.withDefault "" (Form.getFieldAsString nameField form).value

        iconValue =
            Maybe.withDefault "" (Form.getFieldAsString iconField form).value
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ formatPrefabs
            , Html.map FormMsg <|
                div [ class "row" ]
                    [ div [ class "col" ]
                        [ FormGroup.input appState form nameField (gettext "Name" appState.locale)
                        ]
                    , div [ class "col text-end" ]
                        [ a
                            [ class "btn btn-danger with-icon"
                            , onClick (Form.RemoveItem "formats" index)
                            , dataCy "document-template-editor_format_remove-button"
                            ]
                            [ faDelete
                            , text (gettext "Remove" appState.locale)
                            ]
                        ]
                    ]
            , Html.map FormMsg <|
                div [ class "row" ]
                    [ div [ class "col" ]
                        [ FormGroup.input appState form iconField (gettext "Icon" appState.locale) ]
                    , div [ class "col" ]
                        [ FormGroup.plainGroup
                            (label [ class "export-link" ] [ fa iconValue, text nameValue ])
                            (gettext "Preview" appState.locale)
                        ]
                    ]
            , FormGroup.listWithCustomMsg appState FormMsg (stepFormView appState cfg stepsField index) form stepsField (gettext "Steps" appState.locale) (gettext "Add step" appState.locale)
            ]
        ]


stepFormView : AppState -> ViewConfig -> String -> Int -> Form FormError DocumentTemplateForm -> Int -> Html Msg
stepFormView appState cfg prefix formatIndex form index =
    let
        stepPrefabs =
            case ( DocumentTemplateForm.isStepEmpty formatIndex index form, cfg.documentTemplateFormatStepPrefabs ) of
                ( True, ActionResult.Success formats ) ->
                    let
                        viewStep step =
                            a
                                [ onClick (FillStep formatIndex index step)
                                , class "btn btn-outline-primary me-1 btn-wide"
                                ]
                                [ text step.name ]
                    in
                    prefabsView appState (List.map viewStep formats)

                _ ->
                    Html.nothing

        nameField =
            prefix ++ "." ++ String.fromInt index ++ ".name"

        optionsField =
            prefix ++ "." ++ String.fromInt index ++ ".options"
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ stepPrefabs
            , Html.map FormMsg <|
                div [ class "row" ]
                    [ div [ class "col-11" ]
                        [ FormGroup.input appState form nameField (gettext "Name" appState.locale) ]
                    , div [ class "col text-end" ]
                        [ a
                            [ class "btn btn-link text-danger"
                            , onClick (Form.RemoveItem prefix index)
                            , dataCy "document-template-editor_step_remove-button"
                            ]
                            [ faDelete
                            ]
                        ]
                    ]
            , Html.map FormMsg <|
                div [ class "input-table" ]
                    [ label [] [ text (gettext "Options" appState.locale) ]
                    , serviceParametersHeader appState optionsField form
                    , FormGroup.list appState (stepOptionFormView appState optionsField) form optionsField "" (gettext "Add option" appState.locale)
                    ]
            ]
        ]


serviceParametersHeader : AppState -> String -> Form FormError a -> Html msg
serviceParametersHeader appState field form =
    if List.isEmpty (Form.getListIndexes field form) then
        Html.nothing

    else
        div [ class "row input-table-header" ]
            [ div [ class "col-5" ] [ text (gettext "Name" appState.locale) ]
            , div [ class "col-6" ] [ text (gettext "Value" appState.locale) ]
            ]


stepOptionFormView : AppState -> String -> Form FormError DocumentTemplateForm -> Int -> Html Form.Msg
stepOptionFormView appState prefix form i =
    let
        name =
            prefix ++ "." ++ String.fromInt i ++ ".key"

        value =
            prefix ++ "." ++ String.fromInt i ++ ".value"

        nameField =
            Form.getFieldAsString name form

        valueField =
            Form.getFieldAsString value form

        ( nameError, nameErrorClass ) =
            FormGroup.getErrors appState nameField (gettext "Name" appState.locale)

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField (gettext "Value" appState.locale)
    in
    div [ class "row mb-2" ]
        [ div [ class "col-5" ]
            [ Input.textInput nameField [ class <| "form-control " ++ nameErrorClass, dataCy "settings_authentication_service_parameter-name" ]
            , nameError
            ]
        , div [ class "col-6" ]
            [ Input.textInput valueField [ class <| "form-control " ++ valueErrorClass, dataCy "settings_authentication_service_parameter-value" ]
            , valueError
            ]
        , div [ class "col-1 text-end" ]
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix i) ] [ faDelete ] ]
        ]


prefabsView : AppState -> List (Html msg) -> Html msg
prefabsView appState prefabButtons =
    Html.viewIf (not (List.isEmpty prefabButtons)) <|
        div [ class "row" ]
            [ div [ class "col" ]
                [ div [ class "py-2 px-3 bg-gray-200 rounded mb-3" ]
                    [ strong [ class "d-block mb-2" ] [ text (gettext "Quick setup" appState.locale) ]
                    , div [] prefabButtons
                    ]
                ]
            ]
