module Public.Routing exposing (..)

import UrlParser exposing (..)


type Route
    = ForgottenPassword
    | Home
    | Login
    | Signup
    | SignupConfirmation String String


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute <| ForgottenPassword) (s "forgotten-password")
    , map (wrapRoute <| Home) top
    , map (wrapRoute <| Login) (s "login")
    , map (wrapRoute <| Signup) (s "signup")
    , map (signupConfirmation wrapRoute) (s "signup-confirmation" </> string </> string)
    ]


signupConfirmation : (Route -> a) -> String -> String -> a
signupConfirmation wrapRoute userId hash =
    SignupConfirmation userId hash |> wrapRoute


toUrl : Route -> List String
toUrl route =
    case route of
        ForgottenPassword ->
            [ "forgotten-password" ]

        Home ->
            []

        Login ->
            [ "login" ]

        Signup ->
            [ "signup" ]

        SignupConfirmation userId hash ->
            [ "signup-confirmation", userId, hash ]
