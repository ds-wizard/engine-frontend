module Wizard.Pages.Users.Edit.Components.AppKeys exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe
import Shared.Components.ActionResultBlock as ActionResultBlock
import Shared.Components.Flash as Flash
import Shared.Components.FontAwesome exposing (faDelete)
import Shared.Components.Modal as Modal
import Shared.Components.Page as Page
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Data.UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Utils.RequestHelpers as RequestHelpers
import Shared.Utils.Setters exposing (setAppKeys)
import Shared.Utils.TimeUtils as TimeUtils
import String.Format as String
import Wizard.Api.AppKeys as AppKeysApi
import Wizard.Api.Models.AppKey exposing (AppKey)
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , appKeys : ActionResult (List AppKey)
    , appKeyToDelete : Maybe AppKey
    , deletingAppKey : ActionResult String
    }


initialModel : UuidOrCurrent -> Model
initialModel uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , appKeys = ActionResult.Loading
    , appKeyToDelete = Nothing
    , deletingAppKey = ActionResult.Unset
    }


type Msg
    = GetAppKeysCompleted (Result ApiError (List AppKey))
    | SetAppKeyToDelete (Maybe AppKey)
    | DeleteAppKey
    | DeleteAppKeyCompleted (Result ApiError ())


fetchData : AppState -> Cmd Msg
fetchData appState =
    AppKeysApi.getAppKeys appState GetAppKeysCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetAppKeysCompleted result ->
            RequestHelpers.applyResult
                { setResult = setAppKeys
                , defaultError = gettext "Unable to get app keys." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        SetAppKeyToDelete appKeyToDelete ->
            ( { model | appKeyToDelete = appKeyToDelete }
            , Cmd.none
            )

        DeleteAppKey ->
            case model.appKeyToDelete of
                Just appKey ->
                    ( { model | deletingAppKey = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg (AppKeysApi.deleteAppKey appState appKey.uuid DeleteAppKeyCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteAppKeyCompleted result ->
            case result of
                Ok _ ->
                    ( { model
                        | appKeyToDelete = Nothing
                        , appKeys = ActionResult.Loading
                        , deletingAppKey = ActionResult.Unset
                      }
                    , Cmd.map cfg.wrapMsg (fetchData appState)
                    )

                Err error ->
                    ( { model | deletingAppKey = ApiError.toActionResult appState (gettext "App key could not be deleted." appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    div []
        [ Page.header (gettext "App Keys" appState.locale) []
        , div [ class "row" ]
            [ div [ class "col-8" ] [ viewAppKeys appState model ]
            , div [ class "col-4" ]
                [ div [ class "col-border-left" ]
                    [ text (gettext "App keys are created when connecting various applications. You can disconnect them by deleting the app key." appState.locale)
                    ]
                ]
            ]
        , viewAppKeyDeleteModal appState model
        ]


viewAppKeys : AppState -> Model -> Html Msg
viewAppKeys appState model =
    ActionResultBlock.view
        { viewContent = viewAppKeysTable appState
        , actionResult = model.appKeys
        , locale = appState.locale
        }


viewAppKeysTable : AppState -> List AppKey -> Html Msg
viewAppKeysTable appState appKeys =
    let
        viewTime time =
            TimeUtils.toReadableDate appState.timeZone time

        viewApiKeyRow : AppKey -> Html Msg
        viewApiKeyRow appKey =
            tr []
                [ td [] [ text appKey.name ]
                , td [ class "text-nowrap" ] [ text (viewTime appKey.createdAt) ]
                , td [ class "text-center px-2" ]
                    [ a [ class "text-danger", onClick (SetAppKeyToDelete (Just appKey)) ]
                        [ faDelete ]
                    ]
                ]
    in
    if List.isEmpty appKeys then
        Flash.info (gettext "You have no app keys." appState.locale)

    else
        table [ class "table table-hover" ]
            [ thead []
                [ tr []
                    [ th [] [ text (gettext "App Key Name" appState.locale) ]
                    , th [] [ text (gettext "Created" appState.locale) ]
                    , th [] []
                    ]
                ]
            , tbody [] (List.map viewApiKeyRow (List.sortBy (String.toLower << .name) appKeys))
            ]


viewAppKeyDeleteModal : AppState -> Model -> Html Msg
viewAppKeyDeleteModal appState model =
    let
        modalContent =
            String.formatHtml (gettext "Are you sure you want to delete %s?" appState.locale)
                [ strong [] [ text (Maybe.unwrap "" .name model.appKeyToDelete) ] ]

        cfg =
            Modal.confirmConfig (gettext "Delete App Key" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.appKeyToDelete)
                |> Modal.confirmConfigActionResult model.deletingAppKey
                |> Modal.confirmConfigAction (gettext "Delete" appState.locale) DeleteAppKey
                |> Modal.confirmConfigCancelMsg (SetAppKeyToDelete Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "app-keys_delete"
    in
    Modal.confirm appState cfg
