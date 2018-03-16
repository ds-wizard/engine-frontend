module Public.Routing exposing (..)

import UrlParser exposing (..)


type Route
    = ForgottenPassword
    | Home
    | Login
    | Signup


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (wrapRoute ForgottenPassword) (s "forgotten-password")
    , map (wrapRoute Home) top
    , map (wrapRoute Login) (s "login")
    , map (wrapRoute Signup) (s "signup")
    ]


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
