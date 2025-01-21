module Wizard.Users.Edit.Components.ActiveSessions exposing
    ( Model
    , Msg
    , Token
    , UpdateConfig
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, span, strong, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Extra exposing (viewIf)
import Maybe.Extra as Maybe
import Shared.Api.Tokens as TokensApi
import Shared.Common.TimeUtils as TimeUtils
import Shared.Components.Badge as Badge
import Shared.Data.ApiKey exposing (ApiKey)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, faSet, faSetFw)
import Shared.Markdown as Markdown
import Shared.Setters exposing (setTokens)
import String.Format as String
import Time
import UserAgent
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (tooltip)
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page


type alias Token =
    ApiKey


type alias Model =
    { tokens : ActionResult (List Token)
    , tokenToRevoke : Maybe Token
    , revokingToken : ActionResult String
    , revokeAllModalOpen : Bool
    , revokingAll : ActionResult String
    }


initialModel : Model
initialModel =
    { tokens = ActionResult.Loading
    , tokenToRevoke = Nothing
    , revokingToken = ActionResult.Unset
    , revokeAllModalOpen = False
    , revokingAll = ActionResult.Unset
    }


type Msg
    = GetTokensComplete (Result ApiError (List Token))
    | SetTokenToRevoke (Maybe Token)
    | RevokeToken
    | DeleteTokenComplete (Result ApiError ())
    | RevokeAllModalOpen Bool
    | RevokeAll
    | DeleteTokensComplete (Result ApiError ())


fetchData : AppState -> Cmd Msg
fetchData appState =
    TokensApi.getTokens appState GetTokensComplete


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetTokensComplete result ->
            applyResult appState
                { setResult = setTokens
                , defaultError = gettext "Unable to get active sessions." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                }

        SetTokenToRevoke mbApiKey ->
            ( { model | tokenToRevoke = mbApiKey }, Cmd.none )

        RevokeToken ->
            case model.tokenToRevoke of
                Just apiKey ->
                    ( { model | revokingToken = ActionResult.Loading }
                    , Cmd.map cfg.wrapMsg (TokensApi.deleteToken apiKey.uuid appState DeleteTokenComplete)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteTokenComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | tokenToRevoke = Nothing
                        , tokens = ActionResult.Loading
                        , revokingToken = ActionResult.Unset
                      }
                    , Cmd.map cfg.wrapMsg <| TokensApi.getTokens appState GetTokensComplete
                    )

                Err error ->
                    ( { model
                        | revokingToken = ApiError.toActionResult appState (gettext "Session could not be revoked." appState.locale) error
                      }
                    , getResultCmd cfg.logoutMsg result
                    )

        RevokeAllModalOpen value ->
            ( { model | revokeAllModalOpen = value }, Cmd.none )

        RevokeAll ->
            ( { model | revokingAll = ActionResult.Loading }
            , Cmd.map cfg.wrapMsg (TokensApi.deleteTokens appState DeleteTokensComplete)
            )

        DeleteTokensComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | revokeAllModalOpen = False
                        , tokens = ActionResult.Loading
                        , revokingAll = ActionResult.Unset
                      }
                    , Cmd.map cfg.wrapMsg <| TokensApi.getTokens appState GetTokensComplete
                    )

                Err error ->
                    ( { model
                        | revokingAll = ApiError.toActionResult appState (gettext "Failed to revoke all active sessions." appState.locale) error
                      }
                    , getResultCmd cfg.logoutMsg result
                    )


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewActiveSessions appState model) model.tokens


viewActiveSessions : AppState -> Model -> List Token -> Html Msg
viewActiveSessions appState model tokens =
    let
        activeSessions =
            List.sortBy (Time.posixToMillis << .createdAt) tokens

        revokeAllAction =
            if List.length tokens > 1 then
                button
                    [ class "btn btn-outline-danger with-icon"
                    , onClick (RevokeAllModalOpen True)
                    ]
                    [ faSet "activeSession.revoke" appState
                    , text (gettext "Revoke all" appState.locale)
                    ]

            else
                emptyNode
    in
    div []
        [ div [ class "row" ]
            [ div [ class "col-8" ]
                [ Page.headerWithGuideLink appState (gettext "Active Sessions" appState.locale) GuideLinks.profileActiveSessions ]
            ]
        , div [ class "row" ]
            [ div [ class "col-8" ] [ div [ class "list-group" ] (List.map (viewActiveSession appState) activeSessions) ]
            , div [ class "col-4" ]
                [ div [ class "col-border-left" ]
                    [ Markdown.toHtml [] (gettext "This is a list of active sessions logged into your account.\n\nIf you don't recognize any, you should revoke them." appState.locale)
                    , revokeAllAction
                    ]
                ]
            ]
        , viewActiveSessionRevokeModal appState model
        , viewRevokeAllModal appState model
        ]


viewActiveSession : AppState -> Token -> Html Msg
viewActiveSession appState token =
    let
        isTDK =
            String.startsWith "dsw-tdk/" token.userAgent

        icon =
            if isTDK then
                faSetFw "userAgent.tdk" appState

            else if UserAgent.isMobile (UserAgent.getOS token.userAgent) then
                faSetFw "userAgent.mobile" appState

            else
                faSetFw "userAgent.desktop" appState

        sessionLabel =
            if isTDK then
                strong [] [ text "DSW TDK" ]

            else
                tokenToHtml appState token
    in
    div [ class "list-group-item d-flex align-items-baseline" ]
        [ div [ class "me-2" ] [ icon ]
        , div [ class "flex-grow-1" ]
            [ div [ class "d-flex align-items-center" ]
                [ sessionLabel
                , viewIf token.currentSession <|
                    Badge.info [ class "ms-2" ] [ text (gettext "current" appState.locale) ]
                ]
            , div []
                [ text
                    (String.format (gettext "Logged in on %s" appState.locale)
                        [ TimeUtils.toReadableDateTime appState.timeZone token.createdAt ]
                    )
                ]
            ]
        , viewIf (not token.currentSession) <|
            a (class "light-danger" :: onClick (SetTokenToRevoke (Just token)) :: tooltip (gettext "Revoke session" appState.locale))
                [ faSet "activeSession.revoke" appState ]
        ]


viewActiveSessionRevokeModal : AppState -> Model -> Html Msg
viewActiveSessionRevokeModal appState model =
    let
        content =
            case model.tokenToRevoke of
                Just token ->
                    String.formatHtml (gettext "Are you sure you want to revoke %s (logged in on %s)?" appState.locale)
                        [ tokenToHtml appState token
                        , text (TimeUtils.toReadableDateTime appState.timeZone token.createdAt)
                        ]

                Nothing ->
                    []

        cfg =
            Modal.confirmConfig (gettext "Revoke Active Session" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible (Maybe.isJust model.tokenToRevoke)
                |> Modal.confirmConfigActionResult model.revokingToken
                |> Modal.confirmConfigAction (gettext "Revoke" appState.locale) RevokeToken
                |> Modal.confirmConfigCancelMsg (SetTokenToRevoke Nothing)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "active-session_revoke"
    in
    Modal.confirm appState cfg


viewRevokeAllModal : AppState -> Model -> Html Msg
viewRevokeAllModal appState model =
    let
        content =
            [ text (gettext "Are you sure you want to revoke all active sessions? This will log you out of all devices except for the current one." appState.locale) ]

        cfg =
            Modal.confirmConfig (gettext "Revoke All Active Sessions" appState.locale)
                |> Modal.confirmConfigContent content
                |> Modal.confirmConfigVisible model.revokeAllModalOpen
                |> Modal.confirmConfigActionResult model.revokingAll
                |> Modal.confirmConfigAction (gettext "Revoke all" appState.locale) RevokeAll
                |> Modal.confirmConfigCancelMsg (RevokeAllModalOpen False)
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "active-session_revoke-all"
    in
    Modal.confirm appState cfg


tokenToHtml : AppState -> Token -> Html msg
tokenToHtml appState token =
    span [] <|
        String.formatHtml
            (gettext "%s on %s" appState.locale)
            [ strong [] [ text (UserAgent.browserToString (UserAgent.getBrowser token.userAgent)) ]
            , strong [] [ text (UserAgent.osToString (UserAgent.getOS token.userAgent)) ]
            ]
