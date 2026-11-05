module Registry.Routes exposing
    ( Route(..)
    , isAllowed
    , isPublicOnly
    , kindSegment
    , parse
    , redirect
    , toUrl
    )

{-| The registry's addresses.

What is published is at `/knowledge-models`, `/document-templates`, `/locales`
and `/mail-template-locales`; one version of a knowledge model or a template at
`/<kind>/<id>:<version>`, which is also how a wizard asks for it. A
translation has no page of its own. The links the registry e-mails are `/signup/<user>/<hash>`
and `/forgotten-password/<user>/<hash>`.

The addresses of the registry before accounts still lead somewhere
(`redirect`).

-}

import Registry.Api.Models.Item exposing (Kind(..))
import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Kind as Kind
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Items Kind
    | ItemDetail Kind String
    | Login (Maybe String)
    | Signup
    | SignupConfirmation String String
    | ForgottenPassword
    | ResetPassword String String
    | Account
    | ApiKeys
    | Users
    | NotFound


parse : AppState -> Url -> Route
parse appState url =
    Maybe.withDefault NotFound (Parser.parse (parser appState) url)


parser : AppState -> Parser (Route -> a) a
parser appState =
    let
        kinds =
            [ ( "knowledge-models", KnowledgeModel ), ( "document-templates", DocumentTemplate ) ]
                ++ (if appState.config.localesEnabled then
                        [ ( "locales", Locale ), ( "mail-template-locales", MailTemplateLocale ) ]

                    else
                        []
                   )

        itemRoutes ( segment, kind ) =
            Parser.map (Items kind) (s segment)
                :: (if Kind.isTranslation kind then
                        []

                    else
                        [ Parser.map (ItemDetail kind) (s segment </> reference) ]
                   )

        signup =
            if appState.config.publicRegistrationEnabled then
                [ Parser.map Signup (s "signup")
                , Parser.map SignupConfirmation (s "signup" </> string </> string)
                ]

            else
                []
    in
    Parser.oneOf
        ([ Parser.map Home top
         , Parser.map Login (s "login" <?> Query.string "originalUrl")
         , Parser.map ForgottenPassword (s "forgotten-password")
         , Parser.map ResetPassword (s "forgotten-password" </> string </> string)
         , Parser.map Account (s "account")
         , Parser.map ApiKeys (s "account" </> s "api-keys")
         , Parser.map Users (s "users")
         ]
            ++ signup
            ++ List.concatMap itemRoutes kinds
        )


{-| Where an address the registry had before accounts went is now: templates
were at `/templates`, an item's identifier had the organization in it
(`org:item:version`, now `org.item:version`, as the server reads it), locales
had a page each, an organization recovered its token at `/forgotten-token` and
edited itself at `/organization`.
-}
redirect : Url -> Maybe String
redirect url =
    case List.filter (not << String.isEmpty) (String.split "/" url.path) of
        [ "templates" ] ->
            Just "/document-templates"

        [ "templates", ref ] ->
            Just ("/document-templates/" ++ withoutOrganization ref)

        [ "forgotten-token" ] ->
            Just "/forgotten-password"

        [ "organization" ] ->
            Just "/account"

        [ "locales", _ ] ->
            Just "/locales"

        [ segment, ref ] ->
            if (segment == "knowledge-models" || segment == "document-templates") && isLegacyReference ref then
                Just ("/" ++ segment ++ "/" ++ withoutOrganization ref)

            else
                Nothing

        _ ->
            Nothing


isLegacyReference : String -> Bool
isLegacyReference ref =
    List.length (String.split ":" (decoded ref)) == 3


withoutOrganization : String -> String
withoutOrganization ref =
    case String.split ":" (decoded ref) of
        [ organizationId, itemId, version ] ->
            organizationId ++ "." ++ itemId ++ ":" ++ version

        _ ->
            ref


decoded : String -> String
decoded segment =
    Maybe.withDefault segment (Url.percentDecode segment)


{-| An `id:version`, percent-decoded: a version is digits and dots, an id
letters, digits, `.`, `_` and `-`, so the colon is the only thing a browser may
have encoded.
-}
reference : Parser (String -> a) a
reference =
    Parser.map (\segment -> Maybe.withDefault segment (Url.percentDecode segment)) string


toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            "/"

        Items kind ->
            "/" ++ kindSegment kind

        ItemDetail kind ref ->
            "/" ++ kindSegment kind ++ "/" ++ ref

        Login (Just originalUrl) ->
            "/login?originalUrl=" ++ Url.percentEncode originalUrl

        Login Nothing ->
            "/login"

        Signup ->
            "/signup"

        SignupConfirmation uuid hash ->
            "/signup/" ++ uuid ++ "/" ++ hash

        ForgottenPassword ->
            "/forgotten-password"

        ResetPassword uuid hash ->
            "/forgotten-password/" ++ uuid ++ "/" ++ hash

        Account ->
            "/account"

        ApiKeys ->
            "/account/api-keys"

        Users ->
            "/users"

        NotFound ->
            "/not-found"


kindSegment : Kind -> String
kindSegment kind =
    case kind of
        KnowledgeModel ->
            "knowledge-models"

        DocumentTemplate ->
            "document-templates"

        Locale ->
            "locales"

        MailTemplateLocale ->
            "mail-template-locales"


{-| Signing in and making an account are for someone who is not signed in.
-}
isPublicOnly : Route -> Bool
isPublicOnly route =
    case route of
        Login _ ->
            True

        Signup ->
            True

        SignupConfirmation _ _ ->
            True

        ForgottenPassword ->
            True

        ResetPassword _ _ ->
            True

        _ ->
            False


{-| The account needs a session; the accounts of everybody an administrator.
What is published is anybody's.
-}
isAllowed : AppState -> Route -> Bool
isAllowed appState route =
    case route of
        Account ->
            AppState.isSignedIn appState

        ApiKeys ->
            AppState.isSignedIn appState

        Users ->
            AppState.isAdmin appState

        _ ->
            True
