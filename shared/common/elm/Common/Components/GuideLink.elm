module Common.Components.GuideLink exposing
    ( GuideLinkConfig
    , guideLink
    )

import Common.Api.Request exposing (ServerInfo)
import Common.Components.FontAwesome exposing (faGuideLink)
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)
import Gettext exposing (gettext)
import Html exposing (Html, a)
import Html.Attributes exposing (class, href, target)


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
