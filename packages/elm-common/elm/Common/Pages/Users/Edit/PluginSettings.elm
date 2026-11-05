module Common.Pages.Users.Edit.PluginSettings exposing
    ( Model
    , Msg
    , fetchData
    , footer
    , initialModel
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Users as UsersApi
import Common.Components.Form as CommonForm
import Common.Components.PluginView as PluginView
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Common.Pages.Users.Components.Section as Section
import Common.Plugins.Plugin exposing (Plugin)
import Common.Plugins.PluginElement as PluginElement
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.Alert as Alert
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , pluginUuid : Uuid
    , pluginSettings : ActionResult String
    , currentPluginSettings : String
    , savingPluginSettings : ActionResult String
    }


initialModel : UuidOrCurrent -> Uuid -> Model
initialModel uuidOrCurrent pluginUuid =
    { uuidOrCurrent = uuidOrCurrent
    , pluginUuid = pluginUuid
    , pluginSettings = Loading
    , currentPluginSettings = ""
    , savingPluginSettings = Unset
    }


pluginSettingsChanged : Model -> Bool
pluginSettingsChanged model =
    Success model.currentPluginSettings /= model.pluginSettings


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState pluginUuid =
    UsersApi.getCurrentUserPluginSettings appState pluginUuid GetPluginSettingsCompleted



-- UPDATE


type Msg
    = GetPluginSettingsCompleted (Result ApiError String)
    | SubmitForm
    | SubmitFormCompleted (Result ApiError ())
    | UpdatePluginSettings String


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetPluginSettingsCompleted result ->
            case result of
                Ok settings ->
                    ( { model | pluginSettings = Success settings, currentPluginSettings = settings }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( { model | pluginSettings = Success "", currentPluginSettings = "" }
                        , Cmd.none
                        )

                    else
                        ( { model | pluginSettings = ApiError.toActionResult appState (gettext "Unable to get plugin settings." appState.locale) error }
                        , Cmd.none
                        )

        SubmitForm ->
            if pluginSettingsChanged model && not (ActionResult.isLoading model.savingPluginSettings) then
                ( { model | savingPluginSettings = Loading }
                , UsersApi.putCurrentPluginSettings appState model.pluginUuid model.currentPluginSettings (cfg.wrapMsg << SubmitFormCompleted)
                )

            else
                ( model, Cmd.none )

        SubmitFormCompleted result ->
            case result of
                Ok _ ->
                    ( { model | savingPluginSettings = Success "" }, CommonForm.reloadWhenSaved )

                Err error ->
                    ( { model | savingPluginSettings = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , CommonForm.focusErrorSummary
                    )

        UpdatePluginSettings newPluginSettings ->
            ( { model | currentPluginSettings = newPluginSettings }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    if pluginSettingsChanged model then
        CommonForm.submitShortcut appState { result = model.savingPluginSettings, onSubmit = SubmitForm }

    else
        Sub.none



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    case AppState.getPlugin appState model.pluginUuid of
        Just plugin ->
            case plugin.connectors.userSettings of
                Just userSettingsConnector ->
                    Section.view appState
                        { title = String.format (gettext "%s Settings" appState.locale) [ plugin.name ]
                        , guideLink = Just GuideLinks.profilePluginSettings
                        , content =
                            Section.actionResult appState
                                (pluginSettingsView appState model plugin userSettingsConnector.element)
                                model.pluginSettings
                        }

                Nothing ->
                    errorView appState (gettext "Plugin has no user settings" appState.locale)

        Nothing ->
            errorView appState (gettext "Plugin not found" appState.locale)


errorView : AppState -> String -> List (Html msg)
errorView appState message =
    Section.view appState
        { title = gettext "Error" appState.locale
        , guideLink = Nothing
        , content = [ Alert.toHtml (Alert.danger message) ]
        }


pluginSettingsView : AppState -> Model -> Plugin -> PluginElement.PluginElement -> String -> List (Html Msg)
pluginSettingsView appState model plugin element pluginSettings =
    [ CommonForm.errorSummary model.savingPluginSettings
    , PluginView.view appState
        plugin.uuid
        element
        -- The settings loaded for this page, not the ones the application
        -- started with: the attribute given last is the one the element gets.
        [ PluginElement.userSettingsValue pluginSettings
        , PluginElement.onUserSettingsValueChange UpdatePluginSettings
        ]
    ]


{-| The unsaved-changes bar, in the settings template's footer slot. It stays
up after a successful save, because the application reloads.
-}
footer : AppState -> Model -> Maybe (Html Msg)
footer appState model =
    if ActionResult.isSuccess model.pluginSettings then
        Just
            -- A successful save reloads the application: the bar goes
            -- first, showing the check on its Save.
            (CommonForm.unsavedBar appState
                { changed = pluginSettingsChanged model && not (ActionResult.isSuccess model.savingPluginSettings)
                , result = model.savingPluginSettings
                , onSave = SubmitForm
                , onDiscard = Nothing
                }
            )

    else
        Nothing
