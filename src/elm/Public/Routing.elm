module Public.Routing exposing (..)

import UrlParser exposing (..)


type Route
    = ForgottenPassword
    | ForgottenPasswordConfirmation String String
    | Home
    | Login
    | Signup
    | SignupConfirmation String String


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute <| ForgottenPassword) (s "forgotten-password")
    , map (forgottenPasswordConfirmation wrapRoute) (s "forgotten-password" </> string </> string)
    , map (wrapRoute <| Home) top
    , map (wrapRoute <| Login) (s "login")
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

        Home ->
            []

        Login ->
            [ "login" ]

        Signup ->
            [ "signup" ]

        SignupConfirmation userId hash ->
            [ "signup-confirmation", userId, hash ]
