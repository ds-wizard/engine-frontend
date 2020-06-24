module Wizard.Registry.Routing exposing (isAllowed, parsers, toUrl)

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Registry.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "registry" appState
    in
    [ map (registrySignupConfirmation wrapRoute) (s moduleRoot </> s (lr "registry.signupConfirmation" appState) </> string </> string)
    ]


registrySignupConfirmation : (Route -> a) -> String -> String -> a
registrySignupConfirmation wrapRoute organizationId hash =
    wrapRoute <| RegistrySignupConfirmationRoute organizationId hash


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "registry" appState
    in
    case route of
        RegistrySignupConfirmationRoute organizationId hash ->
            [ moduleRoot, lr "registry.signupConfirmation" appState, organizationId, hash ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        RegistrySignupConfirmationRoute _ _ ->
            Perm.hasPerm session Perm.settings
