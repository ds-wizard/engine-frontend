module Common.Pages.Settings.Users exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.UsersSettings as UsersSettings exposing (UsersSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model UsersSettings UsersSettings


type alias Msg =
    SettingsPage.Msg UsersSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel UsersSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState UsersSettings.section UsersSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = UsersSettings.section
        , initForm = UsersSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = UsersSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Users" appState.locale
        , description = Nothing
        , guideLink = Nothing
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError UsersSettings -> Html Msg
formView appState form =
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ Field.textarea appState form "affiliations" (gettext "Affiliations" appState.locale)
                |> Field.withHint (gettext "Affiliation options will be used to help users choose their affiliation while signing up or editing their profile. Write one affiliation option per line." appState.locale)
                |> Field.toHtml
            ]


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
