module Wizard.Pages.Settings.PrivacyAndSupport.View exposing (view)

import Compose exposing (compose2)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (placeholder)
import Shared.Components.FormExtra as FormExtra
import Shared.Components.FormGroup as FormGroup
import Shared.Utils.Form.FormError exposing (FormError)
import String.Format as String
import Wizard.Api.Models.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.FontAwesome as FontAwesome
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Pages.Settings.PrivacyAndSupport.Models exposing (Model)
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps PrivacyAndSupportConfig Msg
viewProps =
    { locTitle = gettext "Privacy & Support"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsPrivacyAndSupport
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError PrivacyAndSupportConfig -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.inputAttrs [] appState.locale form "privacyUrl" (gettext "Privacy URL" appState.locale)
        , FormExtra.mdAfter (gettext "URL to page with the privacy policy of the service." appState.locale)
        , FormGroup.inputAttrs [] appState.locale form "termsOfServiceUrl" (gettext "Terms of Service URL" appState.locale)
        , FormExtra.mdAfter (gettext "URL to page with the terms of the service." appState.locale)
        , h3 [] [ text (gettext "Support" appState.locale) ]
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportEmail ] appState.locale form "supportEmail" (gettext "Support Email" appState.locale)
        , FormExtra.mdAfter (gettext "Support email displayed in the help modal." appState.locale)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportSiteName ] appState.locale form "supportSiteName" (gettext "Support Site Name" appState.locale)
        , FormExtra.mdAfter (gettext "Name of the support site where users can report issues related to the service." appState.locale)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportSiteUrl ] appState.locale form "supportSiteUrl" (gettext "Support Site URL" appState.locale)
        , FormExtra.mdAfter (gettext "URL of the support site where users can report issues related to the service." appState.locale)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportSiteIcon ] appState.locale form "supportSiteIcon" (gettext "Support Site Icon" appState.locale)
        , FormExtra.mdAfter (String.format (gettext "Icon of the support site using [Font Awesome](%s)." appState.locale) [ FontAwesome.fontAwesomeLink ])
        ]
