module Wizard.Pages.Users.Edit.Components.ConnectedAccounts exposing
    ( Model
    , Msg
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)
import Common.Api.Models.UserIdentity as UserIdentity exposing (UserIdentity)
import Common.Components.Flash as Flash
import Common.Components.FontAwesome exposing (faDisconnectAccount)
import Common.Components.Modal as Modal
import Common.Components.Page as Page
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Ports.LocalStorage as LocalStorage
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html, a, div, p, strong, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as E
import Maybe.Extra as Maybe
import String.Format as String
import Uuid
import Wizard.Api.OpenIdClients as OpenIdClientsApi
import Wizard.Api.Users as UsersApi
import Wizard.Components.ExternalLoginButton as ExternalLoginButton
import Wizard.Data.AppState exposing (AppState)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


type alias Model =
    { userIdentities : ActionResult (List UserIdentity)
    , userIdentityToDisconnect : Maybe UserIdentity
    , disconnectingIdentity : ActionResult String
    }


initialModel : Model
initialModel =
    { userIdentities = ActionResult.Loading
    , userIdentityToDisconnect = Nothing
    , disconnectingIdentity = ActionResult.Unset
    }


type Msg
    = GetUserIdentitiesCompleted (Result ApiError (List UserIdentity))
    | ExternalLoginOpenId OpenIdClient
    | ShowHideDisconnectUserIdentity (Maybe UserIdentity)
    | DisconnectUserIdentity
    | DisconnectUserIdentityCompleted (Result ApiError ())


fetchData : AppState -> Cmd Msg
fetchData appState =
    UsersApi.getCurrentUserIdentities appState GetUserIdentitiesCompleted


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetUserIdentitiesCompleted result ->
            case result of
                Ok userIdentities ->
                    ( { model | userIdentities = ActionResult.Success userIdentities }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | userIdentities = ApiError.toActionResult appState (gettext "Failed to load connected accounts" appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ExternalLoginOpenId openIdServiceConfig ->
            let
                saveCmd =
                    saveOriginalUrlCmd (Just (Routing.toUrl Routes.usersEditConnectedAccounts))

                redirectCmd =
                    Navigation.load (OpenIdClientsApi.requestUrl appState openIdServiceConfig)
            in
            ( model, Cmd.batch [ saveCmd, redirectCmd ] )

        ShowHideDisconnectUserIdentity mbUserIdentity ->
            ( { model | userIdentityToDisconnect = mbUserIdentity }
            , Cmd.none
            )

        DisconnectUserIdentity ->
            case model.userIdentityToDisconnect of
                Just userIdentity ->
                    let
                        cmd =
                            UsersApi.deleteUserIdentity appState userIdentity.uuid (cfg.wrapMsg << DisconnectUserIdentityCompleted)
                    in
                    ( { model | disconnectingIdentity = ActionResult.Loading }, cmd )

                Nothing ->
                    ( model, Cmd.none )

        DisconnectUserIdentityCompleted result ->
            case result of
                Ok _ ->
                    let
                        userIdentityUuid =
                            Maybe.unwrap Uuid.nil .uuid model.userIdentityToDisconnect

                        filterUserIdentities =
                            List.filter (\ui -> ui.uuid /= userIdentityUuid)
                    in
                    ( { model
                        | disconnectingIdentity = ActionResult.Success (gettext "Account disconnected successfully." appState.locale)
                        , userIdentityToDisconnect = Nothing
                        , userIdentities = ActionResult.map filterUserIdentities model.userIdentities
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | disconnectingIdentity = ApiError.toActionResult appState (gettext "Failed to disconnect account." appState.locale) error
                      }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


saveOriginalUrlCmd : Maybe String -> Cmd msg
saveOriginalUrlCmd originalUrl =
    case originalUrl of
        Just url ->
            LocalStorage.setItem "wizard/originalUrl" (E.string url)

        Nothing ->
            Cmd.none


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (connectedAccountsView appState model) model.userIdentities


connectedAccountsView : AppState -> Model -> List UserIdentity -> Html Msg
connectedAccountsView appState model userIdentities =
    div [ class "users-edit-connected-accounts" ]
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ Page.header (gettext "Connected Accounts" appState.locale) []
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col-8" ]
                [ viewConnectedAccounts appState userIdentities
                , viewExternalLoginButtons appState
                ]
            ]
        , viewDisconnectUserIdentityModal appState model
        ]


viewConnectedAccounts : AppState -> List UserIdentity -> Html Msg
viewConnectedAccounts appState userIdentities =
    if List.isEmpty userIdentities then
        Flash.info (gettext "You have not connected any external accounts." appState.locale)

    else
        table [ class "table table-hover" ]
            [ thead []
                [ tr []
                    [ th [] [ text (gettext "Service" appState.locale) ]
                    , th [] [ text (gettext "Account" appState.locale) ]
                    , th [] []
                    ]
                ]
            , tbody [] (List.map (viewConnectedAccount appState) (List.sortWith UserIdentity.compare userIdentities))
            ]


viewConnectedAccount : AppState -> UserIdentity -> Html Msg
viewConnectedAccount appState userIdentity =
    let
        service =
            { uuid = userIdentity.providerUuid
            , name = userIdentity.providerName
            , style = userIdentity.providerStyle
            }
    in
    tr []
        [ td [] [ ExternalLoginButton.viewAsBadge service ]
        , td [] [ text (UserIdentity.visibleIdentifier userIdentity) ]
        , td [ class "text-end" ]
            [ a
                (class "text-danger p-0"
                    :: onClick (ShowHideDisconnectUserIdentity (Just userIdentity))
                    :: tooltipLeft (gettext "Disconnect" appState.locale)
                )
                [ faDisconnectAccount ]
            ]
        ]


viewExternalLoginButtons : AppState -> Html Msg
viewExternalLoginButtons appState =
    let
        viewExternalLoginButtonOpenId openIdService =
            ExternalLoginButton.view
                { onClick = ExternalLoginOpenId openIdService
                , service = openIdService
                }

        externalLoginButtons =
            List.map viewExternalLoginButtonOpenId appState.config.authentication.external.services
    in
    div [ class "mt-5" ]
        [ strong [ class "mb-2 d-block" ] [ text (gettext "Connect Account" appState.locale) ]
        , div [ class "w-100" ] externalLoginButtons
        ]


viewDisconnectUserIdentityModal : AppState -> Model -> Html Msg
viewDisconnectUserIdentityModal appState model =
    let
        ( visible, content ) =
            case model.userIdentityToDisconnect of
                Just userIdentity ->
                    ( True
                    , [ p []
                            [ text
                                (String.format (gettext "Are you sure you want to disconnect %s account?" appState.locale)
                                    [ userIdentity.providerName
                                    ]
                                )
                            ]
                      , p [] [ text (gettext "You will no longer be able to use it to log in." appState.locale) ]
                      ]
                    )

                Nothing ->
                    ( False, [] )

        cfg =
            Modal.confirmConfig (gettext "Disconnect Account" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible visible
                |> Modal.confirmConfigActionResult model.disconnectingIdentity
                |> Modal.confirmConfigAction (gettext "Disconnect" appState.locale) DisconnectUserIdentity
                |> Modal.confirmConfigCancelMsg (ShowHideDisconnectUserIdentity Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "disconnect-user-identity"
    in
    Modal.confirm appState cfg
