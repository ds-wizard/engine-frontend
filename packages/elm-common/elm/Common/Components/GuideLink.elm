module Common.Components.GuideLink exposing
    ( button
    , link
    )

{-| A way into the user guide from the page it describes.

Both forms are links, because they go somewhere, and both open in a new tab,
because the guide is read beside the page rather than instead of it. A new tab
is a change of context nobody asked for (WCAG 3.2.5), so it is said twice: an
icon for the eye and "(opens in a new tab)" in the accessible name, translated
here rather than left in the system's English default.

Not an icon with a tooltip: a tooltip is not reachable by touch and an icon
alone is a guess, so the button carries its words.

The URL goes through `GuideLinks.wrap`, so the server can
redirect a guide link to the right version of the documentation.

-}

import Common.Api.Request exposing (ServerInfo)
import Common.Data.Context exposing (WithGuideLinks)
import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.Link as Link


{-| A ghost link-button, for a page header's actions — "Learn more in guide".
-}
button : WithGuideLinks a -> ServerInfo -> (GuideLinks -> String) -> Button msg
button appState serverInfo getLink =
    Button.linkGhost (gettext "Learn more in guide" appState.locale) (url appState serverInfo getLink)
        |> Button.withIcon (Icon.solid "fa-circle-question")
        |> Button.external
        |> Button.withExternalNotice (newTabNotice appState)


{-| An inline link, for a sentence or the foot of a form — "Read the guide".
-}
link : WithGuideLinks a -> ServerInfo -> (GuideLinks -> String) -> Html msg
link appState serverInfo getLink =
    Link.link
        { label = gettext "Read the guide" appState.locale
        , url = url appState serverInfo getLink
        }
        |> Link.withIcon (Icon.regular "fa-map")
        |> Link.external
        |> Link.withExternalNotice (newTabNotice appState)
        |> Link.toHtml


url : WithGuideLinks a -> ServerInfo -> (GuideLinks -> String) -> String
url appState serverInfo getLink =
    GuideLinks.wrap serverInfo (getLink appState.guideLinks)


newTabNotice : WithGuideLinks a -> String
newTabNotice appState =
    gettext "(opens in a new tab)" appState.locale
