module Common.Pages.Users.Edit.ActiveSessions exposing
    ( Model
    , Msg
    , fetchData
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ApiKey exposing (ApiKey)
import Common.Api.Tokens as TokensApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Components.Markdown as Markdown
import Common.Data.AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Users.Components.Section as Section
import Common.Utils.RequestHelpers as RequestHelpers
import Common.Utils.Setters exposing (setTokens)
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Time
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Table as Table
import Ui.Text as Text
import UserAgent
import Uuid



-- MODEL


type alias Model =
    { tokens : ActionResult (List ApiKey)
    , tokenToRevoke : Maybe ApiKey
    , revokingToken : ActionResult String
    , revokeAllModalOpen : Bool
    , revokingAll : ActionResult String
    }


initialModel : Model
initialModel =
    { tokens = Loading
    , tokenToRevoke = Nothing
    , revokingToken = Unset
    , revokeAllModalOpen = False
    , revokingAll = Unset
    }


fetchData : AppState -> Cmd Msg
fetchData appState =
    TokensApi.getTokens appState GetTokensComplete



-- UPDATE


type Msg
    = GetTokensComplete (Result ApiError (List ApiKey))
    | SetTokenToRevoke (Maybe ApiKey)
    | RevokeToken
    | DeleteTokenComplete (Result ApiError ())
    | RevokeAllModalOpen Bool
    | RevokeAll
    | DeleteTokensComplete (Result ApiError ())
    | FocusDone


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetTokensComplete result ->
            RequestHelpers.applyResult
                { setResult = setTokens
                , defaultError = gettext "Unable to get active sessions." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        SetTokenToRevoke mbToken ->
            ( { model | tokenToRevoke = mbToken, revokingToken = Unset }, Cmd.none )

        RevokeToken ->
            case model.tokenToRevoke of
                Just token ->
                    ( { model | revokingToken = Loading }
                    , Cmd.map cfg.wrapMsg (TokensApi.deleteToken appState token.uuid DeleteTokenComplete)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteTokenComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | tokenToRevoke = Nothing
                        , tokens = Loading
                        , revokingToken = Unset
                      }
                    , Cmd.map cfg.wrapMsg (Cmd.batch [ fetchData appState, Section.focusHeading FocusDone ])
                    )

                Err error ->
                    ( { model | revokingToken = ApiError.toActionResult appState (gettext "Session could not be revoked." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        RevokeAllModalOpen value ->
            ( { model | revokeAllModalOpen = value, revokingAll = Unset }, Cmd.none )

        RevokeAll ->
            ( { model | revokingAll = Loading }
            , Cmd.map cfg.wrapMsg (TokensApi.deleteTokens appState DeleteTokensComplete)
            )

        DeleteTokensComplete result ->
            case result of
                Ok _ ->
                    ( { model
                        | revokeAllModalOpen = False
                        , tokens = Loading
                        , revokingAll = Unset
                      }
                    , Cmd.map cfg.wrapMsg (Cmd.batch [ fetchData appState, Section.focusHeading FocusDone ])
                    )

                Err error ->
                    ( { model | revokingAll = ApiError.toActionResult appState (gettext "Failed to revoke all active sessions." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        FocusDone ->
            ( model, Cmd.none )



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    Section.view appState
        { title = gettext "Active Sessions" appState.locale
        , guideLink = Just GuideLinks.profileActiveSessions
        , content =
            Markdown.view { headingOffset = 1 } (gettext "This is a list of active sessions logged into your account.\n\nIf you don't recognize any, you should revoke them." appState.locale)
                :: Section.actionResult appState (viewActiveSessions appState) model.tokens
                ++ [ viewRevokeModal appState model
                   , viewRevokeAllModal appState model
                   ]
        }


viewActiveSessions : AppState -> List ApiKey -> List (Html Msg)
viewActiveSessions appState tokens =
    let
        revokeAllAction =
            if List.length tokens > 1 then
                Layout.inline
                    [ Button.dangerSecondary (gettext "Revoke all" appState.locale) (RevokeAllModalOpen True)
                        |> Button.withIcon (Icon.solid "fa-ban")
                        |> Button.withTestId "active-session_revoke-all-button"
                        |> Button.toHtml
                    ]

            else
                Html.text ""

        sessionColumn token =
            let
                isTDK =
                    String.startsWith "c-tdk/" token.userAgent

                icon =
                    if isTDK then
                        "fa-terminal"

                    else if UserAgent.isMobile (UserAgent.getOS token.userAgent) then
                        "fa-mobile-screen-button"

                    else
                        "fa-desktop"

                label =
                    if isTDK then
                        "DSW TDK"

                    else
                        tokenLabel appState token

                currentBadge =
                    if token.currentSession then
                        Badge.toHtml (Badge.info (gettext "current" appState.locale))

                    else
                        Html.text ""
            in
            Layout.inline
                [ Icon.toHtml (Icon.solid icon)
                , Text.label label
                , currentBadge
                ]

        loggedInColumn token =
            Html.text (String.format (gettext "Logged in on %s" appState.locale) [ TimeUtils.toReadableDateTime appState.timeZone token.createdAt ])

        actionsColumn token =
            if token.currentSession then
                []

            else
                [ Button.dangerIconOnly (Icon.solid "fa-ban") (gettext "Revoke" appState.locale) (SetTokenToRevoke (Just token))
                    |> Button.withAccessibleName (String.format (gettext "Revoke session %s" appState.locale) [ tokenLabel appState token ])
                    |> Button.withTestId "active-session_revoke-button"
                ]
    in
    [ Layout.stackLoose
        [ revokeAllAction
        , Table.table
            [ Table.column (gettext "Session" appState.locale) sessionColumn
            , Table.column (gettext "Logged in" appState.locale) loggedInColumn
            , Table.actions (gettext "Actions" appState.locale) actionsColumn
            ]
            (List.sortBy (Time.posixToMillis << .createdAt) tokens)
            |> Table.withRowTestId (\row -> "active-sessions_row_" ++ Uuid.toString row.uuid)
            |> Table.toHtml
        ]
    ]


viewRevokeModal : AppState -> Model -> Html Msg
viewRevokeModal appState model =
    let
        question =
            case model.tokenToRevoke of
                Just token ->
                    String.format (gettext "Are you sure you want to revoke %s (logged in on %s)?" appState.locale)
                        [ tokenLabel appState token
                        , TimeUtils.toReadableDateTime appState.timeZone token.createdAt
                        ]

                Nothing ->
                    ""
    in
    ConfirmModal.dangerConfirm appState
        { title = gettext "Revoke Active Session" appState.locale
        , open = Maybe.isJust model.tokenToRevoke
        , body = [ Text.body question ]
        , confirmLabel = gettext "Revoke" appState.locale
        , onConfirm = RevokeToken
        , onClose = SetTokenToRevoke Nothing
        , result = model.revokingToken
        , testId = Just "active-session_revoke"
        }


viewRevokeAllModal : AppState -> Model -> Html Msg
viewRevokeAllModal appState model =
    ConfirmModal.dangerConfirm appState
        { title = gettext "Revoke All Active Sessions" appState.locale
        , open = model.revokeAllModalOpen
        , body = [ Text.body (gettext "Are you sure you want to revoke all active sessions? This will log you out of all devices except for the current one." appState.locale) ]
        , confirmLabel = gettext "Revoke all" appState.locale
        , onConfirm = RevokeAll
        , onClose = RevokeAllModalOpen False
        , result = model.revokingAll
        , testId = Just "active-session_revoke-all"
        }


tokenLabel : AppState -> ApiKey -> String
tokenLabel appState token =
    String.format (gettext "%s on %s" appState.locale)
        [ UserAgent.browserToString (UserAgent.getBrowser token.userAgent)
        , UserAgent.osToString (UserAgent.getOS token.userAgent)
        ]
