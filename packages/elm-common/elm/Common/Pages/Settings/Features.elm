module Common.Pages.Settings.Features exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import Common.Api.Models.Settings.FeaturesSettings as FeaturesSettings exposing (FeaturesSettings)
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
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model FeaturesSettings FeaturesSettings


type alias Msg =
    SettingsPage.Msg FeaturesSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel FeaturesSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState FeaturesSettings.section FeaturesSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = FeaturesSettings.section
        , initForm = FeaturesSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = FeaturesSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = gettext "Features" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsFeatures)
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError FeaturesSettings -> Html Msg
formView appState form =
    Html.map SettingsPage.formMsg <|
        Layout.stack
            ((Field.toggle appState form "toursEnabled" (gettext "Tours" appState.locale)
                |> Field.withHint (gettext "If enabled, Tours help users navigate the application when opening specific screens for the first time." appState.locale)
                |> Field.toHtml
             )
                :: (if appState.extensions.aiAssistant then
                        [ Field.toggle appState form "aiAssistantEnabled" (gettext "AI Assistant" appState.locale)
                            |> Field.withHint (gettext "If enabled, users can ask the AI assistant for help while they work." appState.locale)
                            |> Field.toHtml
                        ]

                    else
                        []
                   )
            )


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
