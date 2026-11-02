module Common.Pages.Settings.PluginSettings exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The settings of one plugin, edited in the plugin's own element.

Where the settings are the defaults, this is all there is. Where they are used
and may be replaced, the page says whether they have been — "Organization
default" or "Overridden" — and saving replaces all of them there; an override
can be given up again (`Common.Api.Models.PluginSettings`).

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.PluginSettings as PluginSettings exposing (PluginValues)
import Common.Api.PluginSettings as PluginSettingsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Form as CommonForm
import Common.Components.Page as Page
import Common.Components.PluginView as PluginView
import Common.Data.AppState as AppState exposing (AppState)
import Common.Pages.Settings.Components.Section as Section
import Common.Plugins.Plugin exposing (Plugin, SettingsConnector)
import Common.Plugins.PluginElement as PluginElement
import Common.Ports.Window as Window
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Layout as Layout
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { pluginUuid : Uuid
    , pluginSettings : ActionResult PluginValues
    , currentPluginSettings : String
    , savingPluginSettings : ActionResult String
    , resetOpen : Bool
    , resetting : ActionResult String
    }


type Msg
    = GetPluginSettingsCompleted (Result ApiError String)
    | UpdatePluginSettings String
    | SavePluginSettings
    | SavePluginSettingsCompleted (Result ApiError ())
    | SetResetOpen Bool
    | Reset
    | ResetCompleted (Result ApiError ())


initialModel : Uuid -> Model
initialModel pluginUuid =
    { pluginUuid = pluginUuid
    , pluginSettings = Loading
    , currentPluginSettings = ""
    , savingPluginSettings = Unset
    , resetOpen = False
    , resetting = Unset
    }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState pluginUuid =
    ( initialModel pluginUuid
    , PluginSettingsApi.getPluginValues appState pluginUuid GetPluginSettingsCompleted
    )


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        GetPluginSettingsCompleted result ->
            case result of
                Ok response ->
                    let
                        settings =
                            PluginSettings.valuesFromResponse response
                    in
                    ( { model | pluginSettings = Success settings, currentPluginSettings = settings.values }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( { model | pluginSettings = Success { values = "", overrideAllowed = Nothing, overridden = Nothing }, currentPluginSettings = "" }
                        , Cmd.none
                        )

                    else
                        ( { model | pluginSettings = ApiError.toActionResult appState (gettext "Unable to get plugin settings." appState.locale) error }
                        , Cmd.none
                        )

        UpdatePluginSettings newSettings ->
            ( { model | currentPluginSettings = newSettings }, Cmd.none )

        SavePluginSettings ->
            ( { model | savingPluginSettings = Loading }
            , PluginSettingsApi.putPluginValues appState model.pluginUuid model.currentPluginSettings SavePluginSettingsCompleted
            )

        SavePluginSettingsCompleted result ->
            case result of
                Ok _ ->
                    ( { model | savingPluginSettings = Success "" }, CommonForm.reloadWhenSaved )

                Err error ->
                    ( { model | savingPluginSettings = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , CommonForm.focusErrorSummary
                    )

        SetResetOpen open ->
            ( { model | resetOpen = open, resetting = Unset }, Cmd.none )

        Reset ->
            ( { model | resetting = Loading }
            , PluginSettingsApi.deletePluginValues appState model.pluginUuid ResetCompleted
            )

        ResetCompleted result ->
            case result of
                Ok _ ->
                    ( { model | resetting = Success "" }, Window.refresh () )

                Err error ->
                    ( { model | resetting = ApiError.toActionResult appState (gettext "Settings could not be reset." appState.locale) error }
                    , Cmd.none
                    )


formChanged : Model -> Bool
formChanged model =
    ActionResult.unwrap False (\settings -> settings.values /= model.currentPluginSettings) model.pluginSettings


isLocked : Model -> Bool
isLocked model =
    ActionResult.unwrap False (\settings -> settings.overrideAllowed == Just False) model.pluginSettings


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if formChanged model && not model.resetOpen && not (isLocked model) then
        CommonForm.submitShortcut appState { result = model.savingPluginSettings, onSubmit = SavePluginSettings }

    else
        Sub.none


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    let
        -- Only a plugin that is on where the user is: one switched off there
        -- has no settings to edit there.
        mbPlugin =
            AppState.getPlugin appState model.pluginUuid
    in
    case ( mbPlugin, Maybe.andThen (.connectors >> .settings) mbPlugin ) of
        ( Just plugin, Just settingsConnector ) ->
            { body =
                [ Layout.stack
                    [ Section.header plugin.name []
                    , CommonForm.errorSummary model.savingPluginSettings
                    , Section.actionResult appState (viewPluginSettings appState plugin settingsConnector) model.pluginSettings
                    , resetModal appState model
                    ]
                ]
            , footer =
                if isLocked model then
                    Nothing

                else
                    Just
                        -- A successful save reloads the application: the bar goes
                        -- first, showing the check on its Save.
                        (CommonForm.unsavedBar appState
                            { changed = formChanged model && not (ActionResult.isSuccess model.savingPluginSettings)
                            , result = model.savingPluginSettings
                            , onSave = SavePluginSettings
                            , onDiscard = Nothing
                            }
                        )
            }

        _ ->
            let
                switchedOff =
                    Maybe.isNothing mbPlugin && List.any ((==) model.pluginUuid << .uuid) appState.plugins
            in
            { body =
                [ Page.error appState
                    (if switchedOff then
                        gettext "This plugin is switched off here, so it has no settings to change." appState.locale

                     else
                        gettext "Plugin not found" appState.locale
                    )
                ]
            , footer = Nothing
            }


viewPluginSettings : AppState -> Plugin -> SettingsConnector -> PluginValues -> Html Msg
viewPluginSettings appState plugin settingsConnector pluginSettings =
    let
        -- Whose settings these are, where they can be replaced.
        note =
            case ( pluginSettings.overridden, pluginSettings.overrideAllowed ) of
                ( _, Just False ) ->
                    [ Alert.toHtml (Alert.info (gettext "The settings of this plugin are managed by the organization and cannot be changed here." appState.locale)) ]

                ( Just True, _ ) ->
                    [ Layout.inlineBetween
                        [ Layout.inline
                            [ Badge.toHtml (Badge.neutral (gettext "Overridden" appState.locale))
                            , Text.body (gettext "This workspace has its own settings of this plugin." appState.locale)
                            ]
                        , Button.secondary (gettext "Reset to organization default" appState.locale) (SetResetOpen True)
                            |> Button.withTestId "plugin-settings-reset"
                            |> Button.toHtml
                        ]
                    ]

                ( Just False, _ ) ->
                    [ Layout.inline
                        [ Badge.toHtml (Badge.neutral (gettext "Organization default" appState.locale))
                        , Text.body (gettext "Saving a change here replaces all of the settings of this plugin for this workspace." appState.locale)
                        ]
                    ]

                ( Nothing, _ ) ->
                    []
    in
    Layout.stack
        (note
            ++ [ -- The settings as loaded here, rather than as the application
                 -- started with, are what the element edits; the later attribute
                 -- wins.
                 PluginView.view appState
                    plugin.uuid
                    settingsConnector.element
                    [ PluginElement.settingValue pluginSettings.values
                    , PluginElement.onSettingsValueChange UpdatePluginSettings
                    ]
               ]
        )


resetModal : AppState -> Model -> Html Msg
resetModal appState model =
    ConfirmModal.dangerConfirm appState
        { title = gettext "Reset to organization default" appState.locale
        , open = model.resetOpen
        , body =
            [ Text.body (gettext "The settings this workspace has saved for this plugin will be removed, and the organization's will apply again." appState.locale) ]
        , confirmLabel = gettext "Reset" appState.locale
        , onConfirm = Reset
        , onClose = SetResetOpen False
        , result = model.resetting
        , testId = Just "plugin-settings-reset-modal"
        }


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (formChanged model && not (ActionResult.isSuccess model.savingPluginSettings))
