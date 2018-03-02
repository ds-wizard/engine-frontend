module Public.Routing exposing (..)

import UrlParser exposing (..)


type Route
    = ForgottenPassword
    | Home
    | Login
    | Signup


parsers : List ( Route, Parser a a )
parsers =
    [ ( ForgottenPassword, s "forgotten-password" )
    , ( Home, top )
    , ( Login, s "login" )
    , ( Signup, s "signup" )
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
