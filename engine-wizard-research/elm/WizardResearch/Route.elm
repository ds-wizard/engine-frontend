module WizardResearch.Route exposing (Route(..), fromUrl, isPublic, replaceUrl, toString)

import Browser.Navigation as Nav
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Utils exposing (flip)
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s, string)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid exposing (Uuid)
import WizardResearch.Route.ProjectRoute as ProjectRoute exposing (ProjectRoute)


type Route
    = AuthCallback String (Maybe String) (Maybe String)
    | Dashboard PaginationQueryString
    | Login
    | Logout
    | ForgottenPassword
    | SignUp
    | NotFound
    | ProjectCreate
    | Project Uuid ProjectRoute


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Parser.parse parser url)


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map AuthCallback (s "auth" </> string </> s "callback" <?> Query.string "error" <?> Query.string "code")
        , Parser.map (PaginationQueryString.wrapRoute Dashboard (Just "name")) (Parser.top <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map ForgottenPassword (s "forgotten-password")
        , Parser.map SignUp (s "sign-up")
        , Parser.map ProjectCreate (s "create-project")
        , Parser.map (flip Project ProjectRoute.Overview) (s "projects" </> uuid)
        , Parser.map (flip Project ProjectRoute.Planning) (s "projects" </> uuid </> s "planning")
        , Parser.map (flip Project ProjectRoute.Starred) (s "projects" </> uuid </> s "starred")
        , Parser.map (flip Project ProjectRoute.Metrics) (s "projects" </> uuid </> s "metrics")
        , Parser.map (flip Project ProjectRoute.Documents) (s "projects" </> uuid </> s "documents")
        , Parser.map (flip Project ProjectRoute.Settings) (s "projects" </> uuid </> s "settings")
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

                ForgottenPassword ->
                    [ "forgotten-password" ]

                SignUp ->
                    [ "sign-up" ]

                Dashboard paginationQueryString ->
                    [ PaginationQueryString.toUrl paginationQueryString ]

                NotFound ->
                    []

                ProjectCreate ->
                    [ "create-project" ]

                Project uuid projectRoute ->
                    [ "projects", Uuid.toString uuid, ProjectRoute.toString projectRoute ]
    in
    "/" ++ String.join "/" (List.filter (not << String.isEmpty) parts)


isPublic : Route -> Bool
isPublic route =
    case route of
        AuthCallback _ _ _ ->
            True

        Login ->
            True

        ForgottenPassword ->
            True

        SignUp ->
            True

        NotFound ->
            True

        _ ->
            False
