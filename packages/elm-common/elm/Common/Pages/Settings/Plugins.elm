module Common.Pages.Settings.Plugins exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The plugins: which are switched on, and where to configure them.

The list is read from `/plugin-settings`, and what is drawn follows from what it
says (`Common.Api.Models.PluginSettings`):

  - Where plugins are made available, every plugin is listed. One that is on and
    has settings can be opened, and where its settings may be replaced further
    down, a second switch says whether they may.
  - Where they are used, only the plugins made available are listed. Each can
    be switched off there, and its settings opened unless they may not be
    replaced, which the row then says.

Saving reloads the application: a plugin switched on has its code to load.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.PluginSettings exposing (PluginState)
import Common.Api.PluginSettings as PluginSettingsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Form as CommonForm
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Plugins.Plugin as Plugin
import Common.Plugins.PluginMetadata exposing (PluginMetadata)
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.AppShell as AppShell
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Field as Field
import Ui.Form as UiForm
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Separator as Separator
import Ui.Text as Text
import Uuid exposing (Uuid)
import Version


{-| `loaded` is what was read; `states` is what is on the page.
-}
type alias Model =
    { loaded : ActionResult (List PluginState)
    , states : List PluginState
    , saving : ActionResult ()
    , forbidOverride : Maybe Uuid
    }


type Msg
    = GetStatesCompleted (Result ApiError (List PluginState))
    | SetEnabled Uuid Bool
    | SetOverrideAllowed Uuid Bool
    | ShowHideForbidOverride (Maybe Uuid)
    | Submit
    | SubmitCompleted (Result ApiError ())
    | Discard
    | FocusDone


initialModel : Model
initialModel =
    { loaded = Loading
    , states = []
    , saving = Unset
    , forbidOverride = Nothing
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel, PluginSettingsApi.getPluginStates appState GetStatesCompleted )


isChanged : Model -> Bool
isChanged model =
    ActionResult.unwrap False ((/=) model.states) model.loaded


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        setState pluginUuid change =
            { model
                | states =
                    List.map
                        (\state ->
                            if state.pluginUuid == pluginUuid then
                                change state

                            else
                                state
                        )
                        model.states
            }
    in
    case msg of
        GetStatesCompleted result ->
            case result of
                Ok states ->
                    ( { model | loaded = Success states, states = states }, Cmd.none )

                Err error ->
                    ( { model | loaded = ApiError.toActionResult appState (gettext "Unable to get plugin settings." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        SetEnabled pluginUuid enabled ->
            ( setState pluginUuid (\state -> { state | enabled = enabled }), Cmd.none )

        -- Forbidding the override removes the settings that have been saved
        -- further down, so it is asked about first; allowing it is not.
        SetOverrideAllowed pluginUuid allowed ->
            if allowed then
                ( setState pluginUuid (\state -> { state | workspaceOverrideAllowed = Just True }), Cmd.none )

            else
                ( { model | forbidOverride = Just pluginUuid }, Cmd.none )

        ShowHideForbidOverride mbPluginUuid ->
            case ( mbPluginUuid, model.forbidOverride ) of
                -- Confirmed.
                ( Just pluginUuid, _ ) ->
                    let
                        newModel =
                            setState pluginUuid (\state -> { state | workspaceOverrideAllowed = Just False })
                    in
                    ( { newModel | forbidOverride = Nothing }, focusOverrideSwitch cfg pluginUuid )

                ( Nothing, Just pluginUuid ) ->
                    ( { model | forbidOverride = Nothing }, focusOverrideSwitch cfg pluginUuid )

                ( Nothing, Nothing ) ->
                    ( model, Cmd.none )

        Submit ->
            ( { model | saving = Loading }
            , PluginSettingsApi.putPluginStates appState model.states (cfg.wrapMsg << SubmitCompleted)
            )

        SubmitCompleted result ->
            case result of
                Ok _ ->
                    ( { model | saving = Success () }, CommonForm.reloadWhenSaved )

                Err error ->
                    ( { model | saving = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , Cmd.batch [ RequestHelpers.getResultCmd cfg.logoutMsg result, CommonForm.focusErrorSummary ]
                    )

        Discard ->
            ( { model | states = ActionResult.withDefault [] model.loaded, saving = Unset }
            , Browser.Dom.focus AppShell.mainHeadingId
                |> Task.attempt (always (cfg.wrapMsg FocusDone))
            )

        FocusDone ->
            ( model, Cmd.none )


focusOverrideSwitch : UpdateConfig Msg msg -> Uuid -> Cmd msg
focusOverrideSwitch cfg pluginUuid =
    Browser.Dom.focus (overrideSwitchId pluginUuid)
        |> Task.attempt (always (cfg.wrapMsg FocusDone))


overrideSwitchId : Uuid -> String
overrideSwitchId pluginUuid =
    "plugin-override-" ++ Uuid.toString pluginUuid


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if isChanged model && Maybe.isNothing model.forbidOverride then
        CommonForm.submitShortcut appState { result = model.saving, onSubmit = Submit }

    else
        Sub.none


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    let
        -- Only the plugins the list names: where plugins are used, the ones
        -- that have not been made available are not there at all.
        plugins =
            appState.pluginMetadata
                |> List.sortBy (String.toLower << .name)
                |> List.filterMap (\metadata -> Maybe.map (Tuple.pair metadata) (List.find ((==) metadata.uuid << .pluginUuid) model.states))

        content _ =
            if List.isEmpty plugins then
                Text.muted (gettext "There are no plugins." appState.locale)

            else
                -- The rules between the plugins carry their own room, so the
                -- column adds none: a gap of the column's and a margin of the
                -- rule's is the same space twice.
                div [ class "c-settings-plugins" ]
                    [ plugins
                        |> List.map (viewPlugin appState)
                        |> List.intersperse Separator.line
                        |> Layout.stack
                    ]
    in
    { body =
        [ Layout.stack
            [ Section.header (gettext "Plugins" appState.locale)
                [ Section.guideLink appState GuideLinks.settingsPlugins ]
            , CommonForm.errorSummary model.saving
            , Section.actionResult appState content model.loaded
            , forbidOverrideModal appState model
            ]
        ]
    , footer =
        Just
            -- A successful save reloads the application: the bar goes
            -- first, showing the check on its Save.
            (CommonForm.unsavedBar appState
                { changed = isChanged model && not (ActionResult.isSuccess model.saving)
                , result = model.saving
                , onSave = Submit
                , onDiscard = Just Discard
                }
            )
    }


viewPlugin : AppState -> ( PluginMetadata, PluginState ) -> Html Msg
viewPlugin appState ( pluginMetadata, state ) =
    let
        pluginUuidString =
            Uuid.toString pluginMetadata.uuid

        -- The settings pages list every plugin that was loaded, whether or not
        -- it is switched off where the user is.
        mbPlugin =
            List.find ((==) pluginMetadata.uuid << .uuid) appState.plugins

        wasLoaded =
            appState.config.plugins
                |> List.find (\p -> p.uuid == pluginMetadata.uuid)
                |> Maybe.unwrap False .enabled

        malformedManifest =
            wasLoaded && Maybe.isNothing mbPlugin

        apiVersionSupported =
            Plugin.isApiVersionSupported pluginMetadata

        hasSettings =
            mbPlugin
                |> Maybe.andThen (.connectors >> .settings)
                |> Maybe.isJust

        settingsManagedElsewhere =
            state.overrideAllowed == Just False

        settingsLink =
            if hasSettings && state.enabled && not settingsManagedElsewhere then
                Button.iconOnlyLink (Icon.solid "fa-gear") (gettext "Plugin settings" appState.locale) (Routes.toUrl appState (Routes.settingsPluginSettings pluginMetadata.uuid))
                    |> Button.withAccessibleName (String.format (gettext "Settings of %s" appState.locale) [ pluginMetadata.name ])
                    |> Button.toHtml

            else
                Html.text ""

        problemBadges =
            Maybe.values
                [ if malformedManifest then
                    Just (Badge.danger (gettext "malformed manifest" appState.locale))

                  else
                    Nothing
                , if apiVersionSupported then
                    Nothing

                  else
                    Just (Badge.danger (String.format (gettext "unsupported plugin API version %s" appState.locale) [ Version.toStringMinor pluginMetadata.pluginApiVersion ]))
                , if state.overridden == Just True then
                    Just (Badge.neutral (gettext "overridden" appState.locale))

                  else
                    Nothing
                ]

        -- Whether the settings may be replaced further down, for a plugin that
        -- is on and has settings, where that can be said at all.
        overrideSwitch =
            case ( state.workspaceOverrideAllowed, hasSettings && state.enabled ) of
                ( Just allowed, True ) ->
                    [ UiForm.nested
                        [ Field.toggle
                            { id = overrideSwitchId pluginMetadata.uuid
                            , label = gettext "Workspaces can override the settings" appState.locale
                            , checked = allowed
                            , onCheck = SetOverrideAllowed pluginMetadata.uuid
                            }
                            |> Field.withHint (gettext "If enabled, each workspace can replace the settings of this plugin with its own." appState.locale)
                            |> Field.toHtml
                        ]
                    ]

                _ ->
                    []

        managedElsewhere =
            if hasSettings && state.enabled && settingsManagedElsewhere then
                [ Text.muted (gettext "Settings are managed by the organization." appState.locale) ]

            else
                []
    in
    Layout.stack
        (Layout.inlineBetween
            [ Field.toggle
                { id = "plugin-" ++ pluginUuidString
                , label = pluginMetadata.name
                , checked = state.enabled
                , onCheck = SetEnabled pluginMetadata.uuid
                }
                -- The version beside the name, where it is read with it.
                |> Field.withLabelBadge (Badge.neutral (Version.toString pluginMetadata.version))
                |> Field.withHint pluginMetadata.description
                |> Field.withDisabled (malformedManifest || not apiVersionSupported)
                |> Field.toHtml
            , settingsLink
            ]
            :: (if List.isEmpty problemBadges then
                    []

                else
                    [ Layout.inline (List.map Badge.toHtml problemBadges) ]
               )
            ++ managedElsewhere
            ++ overrideSwitch
        )


forbidOverrideModal : AppState -> Model -> Html Msg
forbidOverrideModal appState model =
    let
        name =
            model.forbidOverride
                |> Maybe.andThen (\pluginUuid -> List.find ((==) pluginUuid << .uuid) appState.pluginMetadata)
                |> Maybe.unwrap "" .name
    in
    ConfirmModal.dangerConfirm appState
        { title = gettext "Forbid overriding the settings" appState.locale
        , open = Maybe.isJust model.forbidOverride
        , body =
            [ Text.body (String.format (gettext "Once this is saved, the settings workspaces have saved for %s are removed, and every workspace uses the organization's." appState.locale) [ name ]) ]
        , confirmLabel = gettext "Forbid" appState.locale
        , onConfirm = ShowHideForbidOverride model.forbidOverride
        , onClose = ShowHideForbidOverride Nothing
        , result = Unset
        , testId = Just "plugin-forbid-override"
        }


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (isChanged model && not (ActionResult.isSuccess model.saving))
