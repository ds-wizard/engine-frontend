module Wizard.Pages.Public.Routing exposing
    ( parsers
    , toUrl
    )

import Url exposing (percentEncode)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string, top)
import Url.Parser.Query as Query
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    if Admin.isEnabled appState.config.admin then
        -- Admin owns authentication, the Wizard has no authentication pages of its own. Only the
        -- app root is kept, it redirects to the Admin login when the user is not logged in.
        [ map (wrapRoute << LoginRoute) (top <?> Query.string "originalUrl") ]

    else
        let
            signUpRoutes =
                if appState.config.authentication.internal.registration.enabled then
                    [ map (wrapRoute <| SignupRoute) (s "signup")
                    , map (signupConfirmation wrapRoute) (s "signup" </> string </> string)
                    ]

                else
                    []
        in
        [ map (openIdCallback wrapRoute) (s "open-id" </> string </> s "callback" <?> Query.string "error" <?> Query.string "code" <?> Query.string "session_state" <?> Query.string "state")
        , map (wrapRoute ForgottenPasswordRoute) (s "forgotten-password")
        , map (forgottenPasswordConfirmation wrapRoute) (s "forgotten-password" </> string </> string)
        , map (wrapRoute << LoginRoute) (top <?> Query.string "originalUrl")
        , map (wrapRoute LogoutSuccessful) (s "logout-successful")
        ]
            ++ signUpRoutes


openIdCallback : (Route -> a) -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> a
openIdCallback wrapRoute id error code sessionState state =
    OpenIdCallback id error code sessionState state |> wrapRoute


signupConfirmation : (Route -> a) -> String -> String -> a
signupConfirmation wrapRoute userId hash =
    SignupConfirmationRoute userId hash |> wrapRoute


forgottenPasswordConfirmation : (Route -> a) -> String -> String -> a
forgottenPasswordConfirmation wrapRoute userId hash =
    ForgottenPasswordConfirmationRoute userId hash |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        OpenIdCallback id error code sessionState state ->
            [ "open-id"
            , id
            , "callback"
            , "?error=" ++ Maybe.withDefault "" error ++ "&code=" ++ Maybe.withDefault "" code ++ "&session_state=" ++ Maybe.withDefault "" sessionState ++ "&state=" ++ Maybe.withDefault "" state
            ]

        ForgottenPasswordRoute ->
            [ "forgotten-password" ]

        ForgottenPasswordConfirmationRoute userId hash ->
            [ "forgotten-password", userId, hash ]

        LoginRoute mbOriginalUrl ->
            case mbOriginalUrl of
                Just originalUrl ->
                    [ "/?originalUrl=" ++ percentEncode originalUrl ]

                Nothing ->
                    []

        LogoutSuccessful ->
            [ "logout-successful" ]

        SignupRoute ->
            [ "signup" ]

        SignupConfirmationRoute userId hash ->
            [ "signup", userId, hash ]
