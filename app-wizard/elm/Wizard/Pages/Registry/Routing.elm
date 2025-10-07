module Wizard.Pages.Registry.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Url.Parser exposing ((</>), Parser, map, s, string)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Registry.Routes exposing (Route(..))
import Wizard.Utils.Feature as Feature


moduleRoot : String
moduleRoot =
    "registry"


parsers : (Route -> a) -> List (Parser (a -> c) c)
parsers wrapRoute =
    [ map (registrySignupConfirmation wrapRoute) (s moduleRoot </> s "signup" </> string </> string)
    ]


registrySignupConfirmation : (Route -> a) -> String -> String -> a
registrySignupConfirmation wrapRoute organizationId hash =
    wrapRoute <| RegistrySignupConfirmationRoute organizationId hash


toUrl : Route -> List String
toUrl route =
    case route of
        RegistrySignupConfirmationRoute organizationId hash ->
            [ moduleRoot, "signup", organizationId, hash ]


isAllowed : Route -> AppState -> Bool
isAllowed _ appState =
    Feature.settings appState
