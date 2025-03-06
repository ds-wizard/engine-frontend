module Shared.Api.ExternalLink exposing (externalLinkUrl)

import Shared.AbstractAppState exposing (AbstractAppState)
import Url


externalLinkUrl : AbstractAppState a -> String -> String
externalLinkUrl appState link =
    appState.apiUrl ++ "/external-link?url=" ++ Url.percentEncode link
