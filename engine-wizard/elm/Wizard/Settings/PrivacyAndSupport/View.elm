module Wizard.Settings.PrivacyAndSupport.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div, h3)
import Html.Attributes exposing (placeholder)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lx)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.PrivacyAndSupport.Models exposing (Model)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.PrivacyAndSupport.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.PrivacyAndSupport.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps PrivacyAndSupportConfig Msg
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = compose2 (Html.map FormMsg) formView
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError PrivacyAndSupportConfig -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.inputAttrs [] appState form "privacyUrl" (l_ "form.privacyUrl" appState)
        , FormExtra.mdAfter (l_ "form.privacyUrl.desc" appState)
        , FormGroup.inputAttrs [] appState form "termsOfServiceUrl" (l_ "form.termsOfServiceUrl" appState)
        , FormExtra.mdAfter (l_ "form.termsOfServiceUrl.desc" appState)
        , h3 [] [ lx_ "section.support" appState ]
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportEmail ] appState form "supportEmail" (l_ "form.supportEmail" appState)
        , FormExtra.mdAfter (l_ "form.supportEmail.desc" appState)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportRepositoryName ] appState form "supportRepositoryName" (l_ "form.supportRepositoryName" appState)
        , FormExtra.mdAfter (l_ "form.supportRepositoryName.desc" appState)
        , FormGroup.inputAttrs [ placeholder PrivacyAndSupportConfig.defaultSupportRepositoryUrl ] appState form "supportRepositoryUrl" (l_ "form.supportRepositoryUrl" appState)
        , FormExtra.mdAfter (l_ "form.supportRepositoryUrl.desc" appState)
        ]
