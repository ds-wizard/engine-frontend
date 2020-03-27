module Wizard.Settings.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Settings.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "settings" appState
    in
    [ map (wrapRoute <| AffiliationRoute) (s moduleRoot </> s (lr "settings.affiliation" appState))
    , map (wrapRoute <| AuthRoute) (s moduleRoot </> s (lr "settings.auth" appState))
    , map (wrapRoute <| ClientRoute) (s moduleRoot </> s (lr "settings.client" appState))
    , map (wrapRoute <| FeaturesRoute) (s moduleRoot </> s (lr "settings.features" appState))
    , map (wrapRoute <| InfoRoute) (s moduleRoot </> s (lr "settings.info" appState))
    , map (wrapRoute <| OrganizationRoute) (s moduleRoot </> s (lr "settings.organization" appState))
    ]


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "settings" appState
    in
    case route of
        AuthRoute ->
            [ moduleRoot, lr "settings.auth" appState ]

        AffiliationRoute ->
            [ moduleRoot, lr "settings.affiliation" appState ]

        ClientRoute ->
            [ moduleRoot, lr "settings.client" appState ]

        FeaturesRoute ->
            [ moduleRoot, lr "settings.features" appState ]

        InfoRoute ->
            [ moduleRoot, lr "settings.info" appState ]

        OrganizationRoute ->
            [ moduleRoot, lr "settings.organization" appState ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed _ maybeJwt =
    hasPerm maybeJwt Perm.settings
