module Common.Components.GuideLink exposing
    ( GuideLinkConfig
    , guideLink
    , readTheGuideLink
    )

import Common.Api.Request exposing (ServerInfo)
import Common.Components.FontAwesome exposing (faGuide, faGuideLink)
import Common.Components.Tooltip exposing (tooltipLeft)
import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)
import Gettext exposing (gettext)
import Html exposing (Html, a, span, text)
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
        (href (getUrl cfg)
            :: class "guide-link"
            :: target "_blank"
            :: tooltipLeft (gettext "Learn more in guide" cfg.locale)
        )
        [ faGuideLink ]


readTheGuideLink : GuideLinkConfig -> Html msg
readTheGuideLink cfg =
    a [ href (getUrl cfg), target "_blank", class "text-decoration-underline" ]
        [ faGuide, span [ class "ms-1" ] [ text (gettext "Read the guide" cfg.locale) ] ]


getUrl : GuideLinkConfig -> String
getUrl cfg =
    GuideLinks.wrap cfg.serverInfo (cfg.getLink cfg.guideLinks)
