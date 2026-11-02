module Common.Pages.Settings.Authentication exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.AuthenticationSettings as AuthenticationSettings exposing (AuthenticationSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.AuthenticationSettingsForm as AuthenticationSettingsForm exposing (AuthenticationSettingsForm)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Form as UiForm
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model AuthenticationSettings AuthenticationSettingsForm


type alias Msg =
    SettingsPage.Msg AuthenticationSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel AuthenticationSettingsForm.initEmpty


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState AuthenticationSettings.section AuthenticationSettingsForm.initEmpty


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = AuthenticationSettings.section
        , initForm = AuthenticationSettingsForm.init
        , formToValue = \form _ -> AuthenticationSettingsForm.toSettings form
        , formValidation = AuthenticationSettingsForm.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Authentication" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsAuthentication)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError AuthenticationSettingsForm -> Html Msg
formView appState form =
    let
        twoFactorAuthEnabled =
            Maybe.withDefault False (Form.getFieldAsBool "twoFactorAuthEnabled" form).value

        twoFactorInputs =
            if twoFactorAuthEnabled then
                [ UiForm.nested
                    [ Field.number appState form "twoFactorAuthCodeLength" (gettext "Code Length" appState.locale)
                        |> Field.toHtml
                    , Field.number appState form "twoFactorAuthExpiration" (gettext "Expiration" appState.locale)
                        |> Field.withHintMarkdown (gettext "Expiration time of the authentication code in **seconds**." appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            ([ Field.toggle appState form "registrationEnabled" (gettext "Registration" appState.locale)
                |> Field.withHint (gettext "If enabled, users can create new internal accounts directly in the instance." appState.locale)
                |> Field.toHtml
             , Field.toggle appState form "nonAdminLoginEnabled" (gettext "Non-Admin Login" appState.locale)
                |> Field.withHint (gettext "If enabled, all users can use internal login using email address and password. Otherwise, only admin users can use internal login, and other users must use an external identity provider." appState.locale)
                |> Field.toHtml
             , Field.toggle appState form "twoFactorAuthEnabled" (gettext "Two-Factor Authentication" appState.locale)
                |> Field.withHint (gettext "If enabled, users first enter a username and password at login, and then they receive a one-time code to confirm the login on their email." appState.locale)
                |> Field.toHtml
             ]
                ++ twoFactorInputs
                ++ [ Field.hours appState form "sessionExpiration" (gettext "Session Expiration" appState.locale)
                        |> Field.withHintMarkdown (gettext "Expiration time of the user session in **hours**. Changing this value does not affect existing sessions." appState.locale)
                        |> Field.toHtml
                   , Field.hours appState form "userEmailLinkExpiration" (gettext "User Email Link Expiration" appState.locale)
                        |> Field.withHintMarkdown (gettext "Expiration time of user email links (e.g., password reset, email confirmation) in **hours**." appState.locale)
                        |> Field.toHtml
                   ]
            )


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
