module WizardResearch.Route exposing (Route(..), fromUrl, isPublic, replaceUrl, toString)

import Browser.Navigation as Nav
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s, string)
import Url.Parser.Query as Query


type Route
    = AuthCallback String (Maybe String) (Maybe String)
    | Dashboard
    | Login
    | Logout
    | NotFound
    | ProjectCreate
    | Project String


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map AuthCallback (s "auth" </> string </> s "callback" <?> Query.string "error" <?> Query.string "code")
        , Parser.map Dashboard Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map ProjectCreate (s "create-project")
        , Parser.map Project (s "projects" </> string)
        ]


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toString route)


toString : Route -> String
toString route =
    let
        parts =
            case route of
                AuthCallback id mbError mbCode ->
                    [ "auth", id, "callback", "?error=" ++ String.fromMaybe mbError ++ "&code=" ++ String.fromMaybe mbCode ]

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Dashboard ->
                    [ "" ]

                NotFound ->
                    []

                ProjectCreate ->
                    [ "create-project" ]

                Project uuid ->
                    [ "projects", uuid ]
    in
    "/" ++ String.join "/" parts


isPublic : Route -> Bool
isPublic route =
    case route of
        AuthCallback _ _ _ ->
            True

        Login ->
            True

        NotFound ->
            True

        _ ->
            False
