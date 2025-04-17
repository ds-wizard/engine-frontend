module Wizard.Locales.Create.View exposing (view)

import ActionResult
import File
import Form exposing (Form)
import Form.Field exposing (FieldValue(..))
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, div, input, label, p, text)
import Html.Attributes exposing (accept, class, disabled, id, name, type_)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Locales.Common.LocaleCreateForm exposing (LocaleCreateForm)
import Wizard.Locales.Create.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Locales.Create.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView appState model (File.name file)

                Nothing ->
                    dropzone appState model

        fileGroup =
            div [ class "form-group" ]
                [ label [] [ text (gettext "PO File" appState.locale) ]
                , content
                ]

        formView =
            Html.map FormMsg <|
                div []
                    [ FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
                    , FormGroup.input appState model.form "description" <| gettext "Description" appState.locale
                    , FormGroup.input appState model.form "code" <| gettext "Language Code" appState.locale
                    , FormGroup.input appState model.form "localeId" <| gettext "Locale ID" appState.locale
                    , FormExtra.textAfter <| gettext "Locale ID can only contain alphanumeric characters, hyphens, underscores, and dots." appState.locale
                    , versionInputGroup { form = model.form, label = gettext "Locale Version" appState.locale, major = "localeMajor", minor = "localeMinor", patch = "localePatch" }
                    , FormGroup.input appState model.form "license" <| gettext "License" appState.locale
                    , FormGroup.markdownEditor appState model.form "readme" <| gettext "Readme" appState.locale
                    , versionInputGroup { form = model.form, label = gettext "Recommended App Version" appState.locale, major = "appMajor", minor = "appMinor", patch = "appPatch" }
                    ]

        formActions =
            FormActions.view appState
                Cancel
                (ActionButton.ButtonConfig (gettext "Create" appState.locale) model.creatingLocale (FormMsg <| Form.Submit) False)
    in
    div [ detailClass "" ]
        [ Page.headerWithGuideLink appState (gettext "Create Locale" appState.locale) GuideLinks.localesCreate
        , formView
        , fileGroup
        , formActions
        ]


fileView : AppState -> Model -> String -> Html Msg
fileView appState model fileName =
    div [ class "file-view rounded-3" ]
        [ div [ class "file" ]
            [ faSet "import.file" appState
            , div [ class "filename" ]
                [ text fileName
                , a [ disabled (ActionResult.isLoading model.creatingLocale), class "ms-1 text-danger", onClick CancelFile ]
                    [ faSet "_global.remove" appState ]
                ]
            ]
        ]


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-secondary btn-file" ]
            [ text (gettext "Choose a file" appState.locale)
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected), accept ".po" ] []
            ]
        , p [] [ text (gettext "Or just drop it here" appState.locale) ]
        ]


dropzoneAttributes : Model -> List (Attribute Msg)
dropzoneAttributes model =
    let
        cssClass =
            case model.dnd of
                0 ->
                    ""

                _ ->
                    "active"
    in
    [ class ("rounded-3 dropzone " ++ cssClass)
    , id dropzoneId
    , onDragEvent "dragenter" DragEnter
    , onDragEvent "dragover" DragOver
    , onDragEvent "dragleave" DragLeave
    ]


onDragEvent : String -> Msg -> Attribute Msg
onDragEvent event msg =
    custom event <|
        Decode.succeed
            { stopPropagation = True
            , preventDefault = True
            , message = msg
            }


type alias VersionInputGroupConfig =
    { form : Form FormError LocaleCreateForm
    , label : String
    , major : String
    , minor : String
    , patch : String
    }


versionInputGroup : VersionInputGroupConfig -> Html Form.Msg
versionInputGroup cfg =
    let
        majorField =
            Form.getFieldAsString cfg.major cfg.form

        minorField =
            Form.getFieldAsString cfg.minor cfg.form

        patchField =
            Form.getFieldAsString cfg.patch cfg.form

        errorClass =
            case ( majorField.liveError, minorField.liveError, patchField.liveError ) of
                ( Nothing, Nothing, Nothing ) ->
                    ""

                _ ->
                    " is-invalid"
    in
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text cfg.label ]
        , div [ class "version-inputs" ]
            [ Input.baseInput "number" String Form.Text majorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name cfg.major, id cfg.major ]
            , text "."
            , Input.baseInput "number" String Form.Text minorField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name cfg.minor, id cfg.minor ]
            , text "."
            , Input.baseInput "number" String Form.Text patchField [ class <| "form-control" ++ errorClass, Html.Attributes.min "0", name cfg.patch, id cfg.patch ]
            ]
        ]
