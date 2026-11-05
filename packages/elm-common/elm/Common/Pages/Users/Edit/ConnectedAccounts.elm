module Common.Pages.Users.Edit.ConnectedAccounts exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)
import Common.Api.Models.OpenIdRequest exposing (OpenIdRequestResponse)
import Common.Api.Models.UserIdentity as UserIdentity exposing (UserIdentity)
import Common.Api.OpenIdClients as OpenIdClientsApi
import Common.Api.Users as UsersApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.ExternalLoginButton as ExternalLoginButton
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Users.Components.Section as Section
import Common.Ports.LocalStorage as LocalStorage
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html)
import Json.Encode as E
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Table as Table
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { userIdentities : ActionResult (List UserIdentity)
    , userIdentityToDisconnect : Maybe UserIdentity
    , disconnectingIdentity : ActionResult String
    , connectingEntities : Dict String (ActionResult ())
    }


initialModel : Model
initialModel =
    { userIdentities = Loading
    , userIdentityToDisconnect = Nothing
    , disconnectingIdentity = Unset
    , connectingEntities = Dict.empty
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getCurrentUserIdentities appState GetUserIdentitiesCompleted



-- UPDATE


type Msg
    = GetUserIdentitiesCompleted (Result ApiError (List UserIdentity))
    | ExternalLoginOpenId OpenIdClient
    | ExternalLoginOpenIdCompleted Uuid (Result ApiError OpenIdRequestResponse)
    | ShowHideDisconnectUserIdentity (Maybe UserIdentity)
    | DisconnectUserIdentity
    | DisconnectUserIdentityCompleted (Result ApiError ())
    | FocusDone


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUserIdentitiesCompleted result ->
            case result of
                Ok userIdentities ->
                    ( { model | userIdentities = Success userIdentities }, Cmd.none )

                Err error ->
                    ( { model | userIdentities = ApiError.toActionResult appState (gettext "Failed to load connected accounts" appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ExternalLoginOpenId openIdClient ->
            case Dict.get (Uuid.toString openIdClient.uuid) model.connectingEntities of
                Just Loading ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | connectingEntities = Dict.insert (Uuid.toString openIdClient.uuid) Loading model.connectingEntities }
                    , OpenIdClientsApi.request appState openIdClient (cfg.wrapMsg << ExternalLoginOpenIdCompleted openIdClient.uuid)
                    )

        ExternalLoginOpenIdCompleted uuid result ->
            case result of
                Ok openIdRequestResponse ->
                    ( model
                    , Cmd.batch
                        [ LocalStorage.setItem "wizard/originalUrl" (E.string (Routes.toUrl appState Routes.usersEditConnectedAccounts))
                        , LocalStorage.setItem "wizard/state" (E.string openIdRequestResponse.state)
                        , Navigation.load openIdRequestResponse.url
                        ]
                    )

                Err error ->
                    ( { model | connectingEntities = Dict.insert (Uuid.toString uuid) (ApiError.toActionResult appState (gettext "External login failed." appState.locale) error) model.connectingEntities }
                    , Cmd.none
                    )

        ShowHideDisconnectUserIdentity mbUserIdentity ->
            ( { model | userIdentityToDisconnect = mbUserIdentity, disconnectingIdentity = Unset }, Cmd.none )

        DisconnectUserIdentity ->
            case model.userIdentityToDisconnect of
                Just userIdentity ->
                    ( { model | disconnectingIdentity = Loading }
                    , UsersApi.deleteUserIdentity appState userIdentity.uuid (cfg.wrapMsg << DisconnectUserIdentityCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DisconnectUserIdentityCompleted result ->
            case result of
                Ok _ ->
                    let
                        userIdentityUuid =
                            Maybe.unwrap Uuid.nil .uuid model.userIdentityToDisconnect
                    in
                    ( { model
                        | disconnectingIdentity = Success (gettext "Account disconnected successfully." appState.locale)
                        , userIdentityToDisconnect = Nothing
                        , userIdentities = ActionResult.map (List.filter (\ui -> ui.uuid /= userIdentityUuid)) model.userIdentities
                      }
                    , Section.focusHeading (cfg.wrapMsg FocusDone)
                    )

                Err error ->
                    ( { model | disconnectingIdentity = ApiError.toActionResult appState (gettext "Failed to disconnect account." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FocusDone ->
            ( model, Cmd.none )



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    Section.view appState
        { title = gettext "Connected Accounts" appState.locale
        , guideLink = Just GuideLinks.profileConnectedAccounts
        , content =
            Section.actionResult appState (connectedAccountsView appState model) model.userIdentities
                ++ [ viewDisconnectModal appState model ]
        }


connectedAccountsView : AppState -> Model -> List UserIdentity -> List (Html Msg)
connectedAccountsView appState model userIdentities =
    [ successAlert model.disconnectingIdentity
    , viewConnectedAccounts appState userIdentities
    , viewExternalLoginButtons appState model
    ]


successAlert : ActionResult String -> Html msg
successAlert result =
    case result of
        Success message ->
            Alert.toHtml (Alert.success message)

        _ ->
            Html.text ""


viewConnectedAccounts : AppState -> List UserIdentity -> Html Msg
viewConnectedAccounts appState userIdentities =
    if List.isEmpty userIdentities then
        Alert.toHtml (Alert.info (gettext "You have not connected any external accounts." appState.locale))

    else
        let
            disconnectButton userIdentity =
                [ Button.dangerIconOnly (Icon.solid "fa-link-slash") (gettext "Disconnect" appState.locale) (ShowHideDisconnectUserIdentity (Just userIdentity))
                    |> Button.withAccessibleName (String.format "%s: %s" [ gettext "Disconnect" appState.locale, userIdentity.providerName ])
                    |> Button.withTestId "user-identity_disconnect"
                ]
        in
        Table.table
            [ Table.column (gettext "Service" appState.locale) (\identity -> ExternalLoginButton.badge { name = identity.providerName, style = identity.providerStyle })
            , Table.column (gettext "Account" appState.locale) (Html.text << UserIdentity.visibleIdentifier)
            , Table.actions (gettext "Actions" appState.locale) disconnectButton
            ]
            (List.sortWith UserIdentity.compare userIdentities)
            |> Table.withRowTestId (\row -> "connected-accounts_row_" ++ Uuid.toString row.uuid)
            |> Table.toHtml


viewExternalLoginButtons : AppState -> Model -> Html Msg
viewExternalLoginButtons appState model =
    let
        viewExternalLoginButton openIdClient =
            let
                status =
                    Maybe.withDefault Unset (Dict.get (Uuid.toString openIdClient.uuid) model.connectingEntities)
            in
            Layout.inline
                [ ExternalLoginButton.viewLoading (ActionResult.isLoading status)
                    { onClick = ExternalLoginOpenId openIdClient
                    , service = openIdClient
                    }
                , case status of
                    Error err ->
                        Alert.toHtml (Alert.danger err)

                    _ ->
                        Html.text ""
                ]
    in
    Layout.stack
        (Section.subsection (gettext "Connect Account" appState.locale)
            (List.map viewExternalLoginButton appState.config.authentication.services)
        )


viewDisconnectModal : AppState -> Model -> Html Msg
viewDisconnectModal appState model =
    let
        providerName =
            Maybe.unwrap "" .providerName model.userIdentityToDisconnect
    in
    ConfirmModal.dangerConfirm appState
        { title = gettext "Disconnect Account" appState.locale
        , open = Maybe.isJust model.userIdentityToDisconnect
        , body =
            [ Text.body (String.format (gettext "Are you sure you want to disconnect %s account?" appState.locale) [ providerName ])
            , Text.body (gettext "You will no longer be able to use it to log in." appState.locale)
            ]
        , confirmLabel = gettext "Disconnect" appState.locale
        , onConfirm = DisconnectUserIdentity
        , onClose = ShowHideDisconnectUserIdentity Nothing
        , result = model.disconnectingIdentity
        , testId = Just "disconnect-user-identity"
        }
