module Wizard.Pages.Users.Edit.Components.PluginSettings exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Form as Form
import Common.Components.FormResult as FormResult
import Common.Components.Page as Page
import Common.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Common.Ports.Window as Window
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List.Extra as List
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Users as UsersApi
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Plugins.Plugin as Plugin exposing (Plugin)
import Wizard.Plugins.PluginElement as PluginElement


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
    , pluginSettings = ActionResult.Loading
    , currentPluginSettings = ""
    , savingPluginSettings = ActionResult.Unset
    }


pluginSettingsChanged : Model -> Bool
pluginSettingsChanged model =
    ActionResult.Success model.currentPluginSettings /= model.pluginSettings


type Msg
    = GetPluginSettingsCompleted (Result ApiError String)
    | SubmitForm
    | SubmitFormCompleted (Result ApiError ())
    | UpdatePluginSettings String


fetchData : AppState -> Uuid -> Cmd Msg
fetchData appState pluginUuid =
    UsersApi.getCurrentUserPluginSettings appState pluginUuid GetPluginSettingsCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetPluginSettingsCompleted result ->
            case result of
                Ok settings ->
                    ( { model
                        | pluginSettings = ActionResult.Success settings
                        , currentPluginSettings = settings
                      }
                    , Cmd.none
                    )

                Err error ->
                    if ApiError.isNotFound error then
                        ( { model
                            | pluginSettings = ActionResult.Success ""
                            , currentPluginSettings = ""
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | pluginSettings = ApiError.toActionResult appState (gettext "Unable to get plugin settings." appState.locale) error }, Cmd.none )

        SubmitForm ->
            if pluginSettingsChanged model then
                ( { model | savingPluginSettings = ActionResult.Loading }
                , UsersApi.putCurrentPluginSettings appState model.pluginUuid model.currentPluginSettings (cfg.wrapMsg << SubmitFormCompleted)
                )

            else
                ( model, Cmd.none )

        SubmitFormCompleted result ->
            case result of
                Ok _ ->
                    ( model, Window.refresh () )

                Err error ->
                    ( { model | savingPluginSettings = ApiError.toActionResult appState (gettext "Unable to save plugin settings." appState.locale) error }
                    , Cmd.none
                    )

        UpdatePluginSettings newPluginSettings ->
            ( { model | currentPluginSettings = newPluginSettings }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    case List.find ((==) model.pluginUuid << .uuid) (AppState.getPlugins appState) of
        Just plugin ->
            case plugin.connectors.userSettings of
                Just userSettingsConnector ->
                    Page.actionResultView appState (pluginSettingsView appState model plugin userSettingsConnector) model.pluginSettings

                Nothing ->
                    Page.error appState (gettext "Plugin has no user settings" appState.locale)

        Nothing ->
            Page.error appState (gettext "Plugin not found" appState.locale)


pluginSettingsView : AppState -> Model -> Plugin -> Plugin.SettingsConnector -> String -> Html Msg
pluginSettingsView appState model plugin userSettingsConnector pluginSettings =
    let
        formView =
            PluginElement.element userSettingsConnector.element
                [ PluginElement.settingValue (AppState.getPluginSettings appState plugin.uuid)
                , PluginElement.userSettingsValue pluginSettings
                , PluginElement.onUserSettingsValueChange UpdatePluginSettings
                ]

        form =
            Form.initDynamic appState SubmitForm model.savingPluginSettings
                |> Form.setFormView formView
                |> Form.setFormChanged (pluginSettingsChanged model)
                |> Form.setClass "col-8"
                |> Form.viewDynamic
    in
    div []
        [ Page.header (String.format (gettext "%s Settings" appState.locale) [ plugin.name ]) []
        , div [ class "row" ]
            [ FormResult.errorOnlyView model.savingPluginSettings
            , form
            ]
        ]
