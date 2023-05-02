module Wizard.DocumentTemplateEditors.Editor.Components.Settings exposing
    ( CurrentTemplateEditor
    , Model
    , Msg
    , UpdateConfig
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
import Html exposing (Html, a, button, div, label, span, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import List.Extra as List
import Random exposing (Seed)
import Set
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplate.DocumentTemplatePhase as DocumentTemplatePhase
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, fa, faSet)
import Shared.Utils exposing (dispatch, getUuid)
import Uuid
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
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


initialModel : Model
initialModel =
    { currentEditor = GeneralTemplateEditor
    , form = DocumentTemplateForm.initEmpty
    , formListsChanged = False
    , savingForm = ActionResult.Unset
    }


setDocumentTemplate : DocumentTemplateDraftDetail -> Model -> Model
setDocumentTemplate detail model =
    { model | form = DocumentTemplateForm.init detail }


formChanged : Model -> Bool
formChanged model =
    let
        hasChangedFields =
            not <| Set.isEmpty <| Set.remove "readme-preview-active" <| Form.getChangedFields <| model.form
    in
    model.formListsChanged || hasChangedFields


getFormOutput : Model -> Maybe DocumentTemplateForm
getFormOutput =
    Form.getOutput << .form



-- MSG


type Msg
    = FormMsg Form.Msg
    | SetTemplateEditor CurrentTemplateEditor
    | Save
    | PutTemplateCompleted (Result ApiError DocumentTemplateDraftDetail)


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
                    Form.update DocumentTemplateForm.validation formMsg model.form

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
                            , Form.update DocumentTemplateForm.validation uuidFormMsg newForm
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
                        , DocumentTemplateDraftsApi.putDraft
                            cfg.documentTemplateId
                            (DocumentTemplateForm.encode DocumentTemplatePhase.Draft documentTemplateForm)
                            appState
                            (cfg.wrapMsg << PutTemplateCompleted)
                        )

                Nothing ->
                    wrap { model | form = Form.update DocumentTemplateForm.validation Form.Submit model.form }

        PutTemplateCompleted result ->
            case result of
                Ok documentTemplate ->
                    if documentTemplate.id == cfg.documentTemplateId then
                        withSeed
                            ( { model
                                | savingForm = ActionResult.Success ""
                                , form = DocumentTemplateForm.init documentTemplate
                                , formListsChanged = False
                              }
                            , dispatch (cfg.updateDocumentTemplate documentTemplate)
                            )

                    else
                        withSeed
                            ( model
                            , cmdNavigate appState (Routes.documentTemplateEditorDetail documentTemplate.id)
                            )

                Err error ->
                    withSeed
                        ( { model | savingForm = ApiError.toActionResult appState (gettext "Unable to save document template" appState.locale) error }
                        , getResultCmd cfg.logoutMsg result
                        )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
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
                    formViewFormats appState model
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
        , Html.map FormMsg <| FormGroup.input appState model.form "templateId" <| gettext "Template ID" appState.locale
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
            [ FormGroup.listWithHeader appState allowedInputHeader (allowedPackageFormView appState) model.form "allowedPackages" (gettext "Allowed Knowledge Models" appState.locale) (gettext "Add knowledge model" appState.locale)
            ]


allowedPackageFormView : AppState -> Form FormError DocumentTemplateForm -> Int -> Html Form.Msg
allowedPackageFormView appState form index =
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
            [ faSet "_global.delete" appState ]
        ]



-- VIEW - Formats


formViewFormats : AppState -> Model -> Html Msg
formViewFormats appState model =
    Html.map FormMsg <|
        div []
            [ FormGroup.list appState (formatFormView appState) model.form "formats" (gettext "Formats" appState.locale) (gettext "Add format" appState.locale) ]


formatFormView : AppState -> Form FormError DocumentTemplateForm -> Int -> Html Form.Msg
formatFormView appState form index =
    let
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
            [ div [ class "row" ]
                [ div [ class "col" ]
                    [ FormGroup.input appState form nameField (gettext "Name" appState.locale)
                    ]
                , div [ class "col text-end" ]
                    [ a
                        [ class "btn btn-danger with-icon"
                        , onClick (Form.RemoveItem "formats" index)
                        , dataCy "document-template-editor_format_remove-button"
                        ]
                        [ faSet "_global.delete" appState
                        , text (gettext "Remove" appState.locale)
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col" ]
                    [ FormGroup.input appState form iconField (gettext "Icon" appState.locale) ]
                , div [ class "col" ]
                    [ FormGroup.plainGroup
                        (label [ class "export-link" ] [ fa iconValue, text nameValue ])
                        (gettext "Preview" appState.locale)
                    ]
                ]
            , FormGroup.list appState (stepFormView appState stepsField) form stepsField (gettext "Steps" appState.locale) (gettext "Add step" appState.locale)
            ]
        ]


stepFormView : AppState -> String -> Form FormError DocumentTemplateForm -> Int -> Html Form.Msg
stepFormView appState prefix form index =
    let
        nameField =
            prefix ++ "." ++ String.fromInt index ++ ".name"

        optionsField =
            prefix ++ "." ++ String.fromInt index ++ ".options"
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ div [ class "row" ]
                [ div [ class "col-11" ]
                    [ FormGroup.input appState form nameField (gettext "Name" appState.locale) ]
                , div [ class "col text-end" ]
                    [ a
                        [ class "btn btn-link text-danger"
                        , onClick (Form.RemoveItem prefix index)
                        , dataCy "document-template-editor_step_remove-button"
                        ]
                        [ faSet "_global.delete" appState
                        ]
                    ]
                ]
            , div [ class "input-table" ]
                [ label [] [ text (gettext "Options" appState.locale) ]
                , serviceParametersHeader appState optionsField form
                , FormGroup.list appState (stepOptionFormView appState optionsField) form optionsField "" (gettext "Add option" appState.locale)
                ]
            ]
        ]


serviceParametersHeader : AppState -> String -> Form FormError a -> Html msg
serviceParametersHeader appState field form =
    if List.isEmpty (Form.getListIndexes field form) then
        emptyNode

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
            [ a [ class "btn btn-link text-danger", onClick (Form.RemoveItem prefix i) ] [ faSet "_global.delete" appState ] ]
        ]
