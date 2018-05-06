module Public.Routing exposing (..)

import UrlParser exposing (..)


type Route
    = ForgottenPassword
    | ForgottenPasswordConfirmation String String
    | Login
    | Signup
    | SignupConfirmation String String


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute <| ForgottenPassword) (s "forgotten-password")
    , map (forgottenPasswordConfirmation wrapRoute) (s "forgotten-password" </> string </> string)
    , map (wrapRoute <| Login) top
    , map (wrapRoute <| Signup) (s "signup")
    , map (signupConfirmation wrapRoute) (s "signup-confirmation" </> string </> string)
    ]


signupConfirmation : (Route -> a) -> String -> String -> a
signupConfirmation wrapRoute userId hash =
    SignupConfirmation userId hash |> wrapRoute


forgottenPasswordConfirmation : (Route -> a) -> String -> String -> a
forgottenPasswordConfirmation wrapRoute userId hash =
    ForgottenPasswordConfirmation userId hash |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        ForgottenPassword ->
            [ "forgotten-password" ]

        ForgottenPasswordConfirmation userId hash ->
            [ "forgotten-password", userId, hash ]

        Login ->
            []

        Signup ->
            [ "signup" ]

        SignupConfirmation userId hash ->
            [ "signup-confirmation", userId, hash ]
