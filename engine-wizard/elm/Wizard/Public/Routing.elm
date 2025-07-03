module Wizard.Public.Routing exposing
    ( parsers
    , toUrl
    )

import Url exposing (percentEncode)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string, top)
import Url.Parser.Query as Query
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        signUpRoutes =
            if appState.config.authentication.internal.registration.enabled then
                [ map (wrapRoute <| SignupRoute) (s "signup")
                , map (signupConfirmation wrapRoute) (s "signup" </> string </> string)
                ]

            else
                []
    in
    [ map (authCallback wrapRoute) (s "auth" </> string </> s "callback" <?> Query.string "error" <?> Query.string "code" <?> Query.string "session_state")
    , map (wrapRoute ForgottenPasswordRoute) (s "forgotten-password")
    , map (forgottenPasswordConfirmation wrapRoute) (s "forgotten-password" </> string </> string)
    , map (wrapRoute << LoginRoute) (top <?> Query.string "originalUrl")
    , map (wrapRoute LogoutSuccessful) (s "logout-successful")
    ]
        ++ signUpRoutes


authCallback : (Route -> a) -> String -> Maybe String -> Maybe String -> Maybe String -> a
authCallback wrapRoute id error code sessionState =
    AuthCallback id error code sessionState |> wrapRoute


signupConfirmation : (Route -> a) -> String -> String -> a
signupConfirmation wrapRoute userId hash =
    SignupConfirmationRoute userId hash |> wrapRoute


forgottenPasswordConfirmation : (Route -> a) -> String -> String -> a
forgottenPasswordConfirmation wrapRoute userId hash =
    ForgottenPasswordConfirmationRoute userId hash |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        AuthCallback id error code sessionState ->
            [ "auth"
            , id
            , "callback"
            , "?error=" ++ Maybe.withDefault "" error ++ "&code=" ++ Maybe.withDefault "" code ++ "&session_state=" ++ Maybe.withDefault "" sessionState
            ]

        ForgottenPasswordRoute ->
            [ "forgotten-password" ]

        ForgottenPasswordConfirmationRoute userId hash ->
            [ "forgotten-password", userId, hash ]

        LoginRoute mbOriginalUrl ->
            case mbOriginalUrl of
                Just originalUrl ->
                    [ "/?" ++ "originalUrl" ++ "=" ++ percentEncode originalUrl ]

                Nothing ->
                    []

        LogoutSuccessful ->
            [ "logout-successful" ]

        SignupRoute ->
            [ "signup" ]

        SignupConfirmationRoute userId hash ->
            [ "signup", userId, hash ]
