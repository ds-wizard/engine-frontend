module Wizard.Pages.Settings.Authentication.View exposing (view)

import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Data.Role as Role
import Common.Utils.Form.FormError exposing (FormError)
import Compose exposing (compose2)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Authentication.Models exposing (Model)
import Wizard.Pages.Settings.Common.Forms.AuthenticationConfigForm exposing (AuthenticationConfigForm)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState model =
    GenericView.view viewProps appState model


viewProps : GenericView.ViewProps AuthenticationConfigForm Msg
viewProps =
    { locTitle = gettext "Authentication"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsAuthentication
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError AuthenticationConfigForm -> Html Form.Msg
formView appState form =
    let
        internalAuthentication =
            let
                twoFactorAuthEnabled =
                    Maybe.withDefault False (Form.getFieldAsBool "twoFactorAuthEnabled" form).value

                twoFactorInputs =
                    if twoFactorAuthEnabled then
                        div [ class "nested-group" ]
                            [ FormGroup.input appState.locale form "twoFactorAuthCodeLength" (gettext "Code Length" appState.locale)
                            , FormGroup.input appState.locale form "twoFactorAuthExpiration" (gettext "Expiration" appState.locale)
                            , FormExtra.mdAfter (gettext "Expiration time of the authentication code in **seconds**." appState.locale)
                            ]

                    else
                        Html.nothing
            in
            [ h3 [] [ text (gettext "Internal" appState.locale) ]
            , FormGroup.toggle form "registrationEnabled" (gettext "Registration" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, users can create new internal accounts directly in the instance." appState.locale)
            , FormGroup.toggle form "nonAdminLoginEnabled" (gettext "Non-Admin Login" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, all users can use internal login using username and password. Otherwise, only admin users can use internal login, and other users must use an external identity provider." appState.locale)
            , FormGroup.toggle form "twoFactorAuthEnabled" (gettext "Two-Factor Authentication" appState.locale)
            , FormExtra.mdAfter (gettext "If enabled, users first enter a username and password at login, and then they receive a one-time code to confirm the login on their email." appState.locale)
            , twoFactorInputs
            , FormGroup.hours appState.locale form "sessionExpiration" (gettext "Session Expiration" appState.locale)
            , FormExtra.mdAfter (gettext "Expiration time of the user session in **hours**. Changing this value does not affect existing sessions." appState.locale)
            , FormGroup.hours appState.locale form "userEmailLinkExpiration" (gettext "User Email Link Expiration" appState.locale)
            , FormExtra.mdAfter (gettext "Expiration time of user email links (e.g., password reset, email confirmation) in **hours**." appState.locale)
            ]
    in
    div [ class "Authentication" ]
        ([ FormGroup.select appState.locale (Role.options appState) form "defaultRole" (gettext "Default role" appState.locale)
         , FormExtra.mdAfter (gettext "Define the role that is assigned to new users." appState.locale)
         ]
            ++ internalAuthentication
        )
