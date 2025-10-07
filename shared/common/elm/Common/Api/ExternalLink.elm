module Common.Api.ExternalLink exposing (externalLinkUrl)

import Common.Api.Request exposing (ServerInfo)
import Url


externalLinkUrl : ServerInfo -> String -> String
externalLinkUrl serverInfo link =
    serverInfo.apiUrl ++ "/external-link?url=" ++ Url.percentEncode link
