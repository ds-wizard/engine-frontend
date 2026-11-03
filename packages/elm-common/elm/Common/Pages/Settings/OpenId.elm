module Common.Pages.Settings.OpenId exposing
    ( Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)
import Common.Api.OpenIdClients as OpenIdClientsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Table as Table
import Uuid


type alias Model =
    { openIdClients : ActionResult (List OpenIdClient)
    , openIdClientToBeDeleted : Maybe OpenIdClient
    , deletingOpenIdClient : ActionResult String
    }


type Msg
    = GetOpenIdClientsComplete (Result ApiError (List OpenIdClient))
    | ShowHideDeleteOpenIdClient (Maybe OpenIdClient)
    | DeleteOpenIdClient
    | DeleteOpenIdClientComplete (Result ApiError ())
    | FocusDone


initialModel : Model
initialModel =
    { openIdClients = Loading
    , openIdClientToBeDeleted = Nothing
    , deletingOpenIdClient = Unset
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    ( initialModel, OpenIdClientsApi.getOpenIdClients appState GetOpenIdClientsComplete )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetOpenIdClientsComplete result ->
            RequestHelpers.applyResult
                { setResult = \c m -> { m | openIdClients = c }
                , defaultError = gettext "Unable to get OpenID configs." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        ShowHideDeleteOpenIdClient mbOpenIdClient ->
            ( { model | openIdClientToBeDeleted = mbOpenIdClient, deletingOpenIdClient = Unset }, Cmd.none )

        DeleteOpenIdClient ->
            case model.openIdClientToBeDeleted of
                Just openId ->
                    ( { model | deletingOpenIdClient = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        OpenIdClientsApi.deleteOpenIdClient appState openId.uuid DeleteOpenIdClientComplete
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteOpenIdClientComplete result ->
            case result of
                Ok _ ->
                    -- The row and its delete button are gone, so focus goes to
                    -- the section's heading rather than to nothing.
                    ( { model | openIdClientToBeDeleted = Nothing, deletingOpenIdClient = Unset, openIdClients = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        Cmd.batch
                            [ OpenIdClientsApi.getOpenIdClients appState GetOpenIdClientsComplete
                            , Task.attempt (always FocusDone) (Browser.Dom.focus AppShell.mainHeadingId)
                            ]
                    )

                Err error ->
                    ( { model | deletingOpenIdClient = ApiError.toActionResult appState (gettext "OpenID config could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FocusDone ->
            ( model, Cmd.none )


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    { body =
        [ Layout.stack
            [ Section.header (gettext "OpenID" appState.locale)
                [ Section.guideLink appState GuideLinks.settingsOpenId
                , Button.linkPrimary (gettext "Create" appState.locale) (Routes.toUrl appState Routes.settingsOpenIdCreate)
                ]
            , Section.actionResult appState (viewOpenIdClients appState) model.openIdClients
            , viewDeleteModal appState model
            ]
        ]
    , footer = Nothing
    }


viewOpenIdClients : AppState -> List OpenIdClient -> Html Msg
viewOpenIdClients appState openIdClients =
    if List.isEmpty openIdClients then
        Alert.toHtml (Alert.info (gettext "There are no OpenID configurations." appState.locale))

    else
        Table.table
            [ Table.column (gettext "Name" appState.locale) (viewName appState)
            , Table.actions (gettext "Actions" appState.locale) (viewDelete appState)
            ]
            (List.sortBy .name openIdClients)
            |> Table.withRowTestId (\row -> "openid_row_" ++ Uuid.toString row.uuid)
            |> Table.toHtml


viewName : AppState -> OpenIdClient -> Html Msg
viewName appState openIdClient =
    Layout.inline
        [ ExternalLoginButton.icon openIdClient.style
        , Link.link
            { label = openIdClient.name
            , url = Routes.toUrl appState (Routes.settingsOpenIdDetail openIdClient.uuid)
            }
            |> Link.toHtml
        ]


viewDelete : AppState -> OpenIdClient -> List (Button Msg)
viewDelete appState openIdClient =
    [ Button.dangerIconOnly (Icon.solid "fa-trash") (gettext "Delete" appState.locale) (ShowHideDeleteOpenIdClient (Just openIdClient))
        |> Button.withAccessibleName (String.format (gettext "Delete %s" appState.locale) [ openIdClient.name ])
        |> Button.withTestId "openid_delete"
    ]


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    ConfirmModal.deleteWithTestId appState
        { title = gettext "Delete OpenID" appState.locale
        , open = Maybe.isJust model.openIdClientToBeDeleted
        , question =
            String.format (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                [ Maybe.unwrap "" .name model.openIdClientToBeDeleted ]
        , entity = Html.text ""
        , onConfirm = DeleteOpenIdClient
        , onClose = ShowHideDeleteOpenIdClient Nothing
        , result = model.deletingOpenIdClient
        , testId = Just "openid-delete"
        }
