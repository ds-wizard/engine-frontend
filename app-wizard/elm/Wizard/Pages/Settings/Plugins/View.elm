module Wizard.Pages.Settings.Plugins.View exposing (view)

import ActionResult
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faSettingsAlt)
import Common.Components.Form as Form
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltipLeft)
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, button, div, input, label, p, span, text)
import Html.Attributes exposing (checked, class, disabled, type_)
import Html.Events exposing (onCheck, onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Uuid
import Version
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Settings.Plugins.Models exposing (Model)
import Wizard.Pages.Settings.Plugins.Msgs exposing (Msg(..))
import Wizard.Plugins.Plugin as Plugin
import Wizard.Plugins.PluginElement as PluginElement
import Wizard.Plugins.PluginMetadata exposing (PluginMetadata)


view : AppState -> Model -> Html Msg
view appState model =
    let
        form =
            Form.initDynamic appState SubmitPluginsEnabled model.savingPluginsEnabled
                |> Form.setFormView (formView appState model)
                |> Form.setFormChanged model.pluginsChanged
                |> Form.setWide
                |> Form.viewDynamic
    in
    div []
        [ Page.header (gettext "Plugins" appState.locale) []
        , form
        , pluginSettingsModal appState model
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    div [] (List.map (viewPlugin appState model) appState.pluginMetadata)


viewPlugin : AppState -> Model -> PluginMetadata -> Html Msg
viewPlugin appState model pluginMetadata =
    let
        pluginUuidString =
            Uuid.toString pluginMetadata.uuid

        mbPlugin =
            AppState.getPlugin appState pluginMetadata.uuid

        mbSettingsConnector =
            mbPlugin
                |> Maybe.map .connectors
                |> Maybe.andThen .settings

        settingsButton =
            case ( mbSettingsConnector, isEnabled ) of
                ( Just settingsConnector, True ) ->
                    button
                        (class "btn btn-link btn-lg"
                            :: onClick (OpenPluginSettings pluginMetadata.uuid settingsConnector.element)
                            :: tooltipLeft (gettext "Plugin settings" appState.locale)
                        )
                        [ faSettingsAlt ]

                _ ->
                    Html.nothing

        wasLoaded =
            appState.config.plugins
                |> List.find (\p -> p.uuid == pluginMetadata.uuid)
                |> Maybe.unwrap False .enabled

        malformedManifest =
            wasLoaded && Maybe.isNothing mbPlugin

        apiVersionSupported =
            Plugin.isApiVersionSupported pluginMetadata

        isSupported =
            not malformedManifest && apiVersionSupported

        isEnabled =
            Dict.get pluginUuidString model.pluginsEnabled
                |> Maybe.withDefault False

        malformedBadge =
            Html.viewIf malformedManifest <|
                Badge.danger [ class "ms-2" ] [ text (gettext "malformed manifest" appState.locale) ]

        unsupportedBadge =
            Html.viewIf (not apiVersionSupported) <|
                Badge.danger [ class "ms-2" ] [ text (String.format (gettext "unsupported plugin API version %s" appState.locale) [ Version.toStringMinor pluginMetadata.pluginApiVersion ]) ]

        versionBadge =
            Badge.light [ class "ms-2" ] [ text (Version.toString pluginMetadata.version) ]
    in
    div [ class "form-check border-top pt-3 mb-3 d-flex justify-content-between" ]
        [ div []
            [ label [ class "form-check-label form-check-toggle" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , disabled (not isSupported)
                    , checked isEnabled
                    , onCheck (SetPluginEnabled pluginUuidString)
                    ]
                    []
                , span []
                    [ text pluginMetadata.name
                    , versionBadge
                    , malformedBadge
                    , unsupportedBadge
                    ]
                ]
            , p [ class "mt-1 mb-0 text-muted" ] [ text pluginMetadata.description ]
            ]
        , settingsButton
        ]


pluginSettingsModal : AppState -> Model -> Html Msg
pluginSettingsModal appState model =
    let
        content =
            case model.pluginSettingsElement of
                Nothing ->
                    div [] [ text (gettext "No plugin selected." appState.locale) ]

                Just pluginSettingsElement ->
                    ActionResultBlock.view
                        { viewContent =
                            \settings ->
                                div []
                                    [ PluginElement.element pluginSettingsElement
                                        [ PluginElement.settingValue settings
                                        , PluginElement.onSettingsValueChange UpdatePluginSettings
                                        ]
                                    ]
                        , actionResult = model.pluginSettings
                        , locale = appState.locale
                        }

        config =
            Modal.confirmConfig (gettext "Plugin Settings" appState.locale)
                |> Modal.confirmConfigExtraClass "modal-wide"
                |> Modal.confirmConfigVisible (not (ActionResult.isUnset model.pluginSettings))
                |> Modal.confirmConfigContent [ content ]
                |> Modal.confirmConfigCancelMsg ClosePluginSettings
                |> Modal.confirmConfigCancelShortcutMsg ClosePluginSettings
                |> Modal.confirmConfigActionResult model.savingPluginSettings
                |> Modal.confirmConfigAction (gettext "Save" appState.locale) SavePluginSettings
    in
    Modal.confirm appState config
