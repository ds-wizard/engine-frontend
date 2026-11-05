module Common.Pages.Settings.LoginScreen exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.LoginScreenSettings as LoginScreenSettings exposing (LoginScreenSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.AnnouncementFields as AnnouncementFields
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model LoginScreenSettings LoginScreenSettings


type alias Msg =
    SettingsPage.Msg LoginScreenSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel LoginScreenSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState LoginScreenSettings.section LoginScreenSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = LoginScreenSettings.section
        , initForm = LoginScreenSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = LoginScreenSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Login Screen" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsLoginScreen)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError LoginScreenSettings -> Html Msg
formView appState form =
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ Field.markdown appState form "loginInfo" (gettext "Login Info" appState.locale)
                |> Field.withHint (gettext "Additional information displayed on the login screen next to the login form." appState.locale)
                |> Field.toHtml
            , Field.markdown appState form "loginInfoSidebar" (gettext "Sidebar Login Info" appState.locale)
                |> Field.withHint (gettext "Additional information displayed on the login screen underneath the login form." appState.locale)
                |> Field.toHtml
            , AnnouncementFields.view appState
                form
                { field = "announcements"
                , hint = gettext "Announcements are displayed on the login screen, visible to everybody." appState.locale
                }
            ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
