module Registry.Routing exposing (Route(..), toRoute, toString)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type Route
    = ForgottenToken
    | ForgottenTokenConfirmation String String
    | Index
    | KMDetail String
    | Login
    | Organization
    | Signup
    | SignupConfirmation String String
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map ForgottenToken (s "forgotten-token")
        , map ForgottenTokenConfirmation (s "forgotten-token" </> string </> string)
        , map Index top
        , map KMDetail (s "knowledge-models" </> string)
        , map Login (s "login")
        , map Organization (s "organization")
        , map Signup (s "signup")
        , map SignupConfirmation (s "signup" </> string </> string)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (parse routeParser url)


toString : Route -> String
toString route =
    case route of
        ForgottenToken ->
            "/forgotten-token"

        ForgottenTokenConfirmation orgId hash ->
            "/forgotten-token/" ++ orgId ++ "/" ++ hash

        Index ->
            "/"

        KMDetail pkgId ->
            "/knowledge-models/" ++ pkgId

        Login ->
            "/login"

        Organization ->
            "/organization"

        Signup ->
            "/signup"

        SignupConfirmation orgId hash ->
            "/signup/" ++ orgId ++ "/" ++ hash

        _ ->
            "/"
