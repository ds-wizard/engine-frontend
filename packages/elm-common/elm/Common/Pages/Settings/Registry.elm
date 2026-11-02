module Common.Pages.Settings.Registry exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.RegistrySettings as RegistrySettings exposing (RegistrySettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState exposing (AppState)
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
    SettingsPage.Model RegistrySettings RegistrySettings


type alias Msg =
    SettingsPage.Msg RegistrySettings


initialModel : Model
initialModel =
    SettingsPage.initialModel RegistrySettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState RegistrySettings.section RegistrySettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = RegistrySettings.section
        , initForm = RegistrySettings.initForm
        , formToValue = \form _ -> form
        , formValidation = RegistrySettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "DSW Registry" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsRegistry)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError RegistrySettings -> Html Msg
formView appState form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        apiKeyInput =
            if enabled then
                [ UiForm.nested
                    [ Field.secret appState form "apiKey" (gettext "API Key" appState.locale)
                        |> Field.withHintMarkdown (gettext "Fill in an API key of your DSW Registry account. You can create one after signing in to [DSW Registry](https://registry.ds-wizard.org)." appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            ((Field.toggle appState form "enabled" (gettext "Enabled" appState.locale)
                |> Field.withHintMarkdown (gettext "If enabled, you can import knowledge models and document templates directly from [DSW Registry](https://registry.ds-wizard.org)." appState.locale)
                |> Field.toHtml
             )
                :: apiKeyInput
            )


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
