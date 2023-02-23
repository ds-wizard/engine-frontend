module Wizard.Settings.PrivacyAndSupport.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (placeholder)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (compose2)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.FontAwesome as FontAwesome
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.PrivacyAndSupport.Models exposing (Model)


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps PrivacyAndSupportConfig Msg
viewProps =
    { locTitle = gettext "Privacy & Support"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError PrivacyAndSupportConfig -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.inputAttrs [] appState form "privacyUrl" (gettext "Privacy URL" appState.locale)
        , FormExtra.mdAfter (gettext "URL to page with the privacy policy of the service." appState.locale)
        , FormGroup.inputAttrs [] appState form "termsOfServiceUrl" (gettext "Terms of Service URL" appState.locale)
        , FormExtra.mdAfter (gettext "URL to page with the terms of the service." appState.locale)
        , h3 [] [ text (gettext "Support" appState.locale) ]
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportEmail ] appState form "supportEmail" (gettext "Support Email" appState.locale)
        , FormExtra.mdAfter (gettext "Support email displayed in the help modal." appState.locale)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportSiteName ] appState form "supportSiteName" (gettext "Support Site Name" appState.locale)
        , FormExtra.mdAfter (gettext "Name of the support site where users can report issues related to the service." appState.locale)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportSiteUrl ] appState form "supportSiteUrl" (gettext "Support Site URL" appState.locale)
        , FormExtra.mdAfter (gettext "URL of the support site where users can report issues related to the service." appState.locale)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportSiteIcon ] appState form "supportSiteIcon" (gettext "Support Site Icon" appState.locale)
        , FormExtra.mdAfter (String.format (gettext "Icon of the support site using [Font Awesome](%s)." appState.locale) [ FontAwesome.fontAwesomeLink ])
        ]
