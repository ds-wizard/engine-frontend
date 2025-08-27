module Wizard.Locales.Create.View exposing (view)

import Form exposing (Form)
import Form.Field exposing (FieldValue(..))
import Form.Input as Input
import Gettext exposing (gettext)
import Html exposing (Html, div, label, p, text)
import Html.Attributes exposing (class, id, name)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Dropzone as Dropzone
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Locales.Common.LocaleCreateForm exposing (LocaleCreateForm)
import Wizard.Locales.Create.Models exposing (Model)
import Wizard.Locales.Create.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        fileWarning file =
            if Form.isSubmitted model.form && Maybe.isNothing file then
                p [ class "form-text form-text-after text-danger mt-2" ]
                    [ text (gettext "File is required." appState.locale) ]

            else
                Html.nothing

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
        , div [ class "form-group", dataCy "wizard-locale-dropzone" ]
            [ label [] [ text (gettext "Wizard Locale" appState.locale) ]
            , Dropzone.dropzone
                { wrapMsg = WizardContentFileDropzoneMsg
                , buttonText = gettext "Select .po file" appState.locale
                , dropzoneText = gettext "or drop it here" appState.locale
                , fileIcon = Nothing
                }
                model.wizardContentFileDropzone
            , fileWarning model.wizardContent
            ]
        , div [ class "form-group", dataCy "mail-locale-dropzone" ]
            [ label [] [ text (gettext "Mails Locale" appState.locale) ]
            , Dropzone.dropzone
                { wrapMsg = MailContentFileDropzoneMsg
                , buttonText = gettext "Select .po file" appState.locale
                , dropzoneText = gettext "or drop it here" appState.locale
                , fileIcon = Nothing
                }
                model.mailContentFileDropzone
            , fileWarning model.mailContent
            ]
        , formActions
        ]


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
