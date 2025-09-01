module Shared.Components.GuideLink exposing
    ( GuideLinkConfig
    , guideLink
    )

import Gettext exposing (gettext)
import Html exposing (Html, a)
import Html.Attributes exposing (class, href, target)
import Shared.Api.Request exposing (ServerInfo)
import Shared.Components.FontAwesome exposing (faGuideLink)
import Shared.Components.Tooltip exposing (tooltipLeft)
import Shared.Utils.GuideLinks as GuideLinks exposing (GuideLinks)


type alias GuideLinkConfig =
    { guideLinks : GuideLinks
    , locale : Gettext.Locale
    , getLink : GuideLinks -> String
    , serverInfo : ServerInfo
    }


guideLink : GuideLinkConfig -> Html msg
guideLink cfg =
    a
        (href (GuideLinks.wrap cfg.serverInfo (cfg.getLink cfg.guideLinks))
            :: class "guide-link"
            :: target "_blank"
            :: tooltipLeft (gettext "Learn more in guide" cfg.locale)
        )
        [ faGuideLink ]
