module Common.Components.Announcements exposing (view)

{-| The announcements an administrator has written, on the dashboard and on the
login screen.

Each one is a `Ui.Alert` whose tone is its level — info, warning, critical as
danger — with its markdown as the content. The tone is carried by the icon and
the edge as well as the color, so a critical announcement is not told from an
informational one by hue alone (1.4.1).

Warning and critical announcements are `role="alert"`, the same as every other
alert of those tones; info is a status. They are on the page when it loads, so
there is nothing to interrupt.

Headings inside an announcement are demoted below the page's own
(`Common.Components.Markdown`), so an announcement that starts with `# Planned
maintenance` does not become a second page title.

-}

import Common.Api.Models.Announcement exposing (Announcement)
import Common.Api.Models.Announcement.AnnouncementLevel as AnnouncementLevel
import Common.Components.Markdown as Markdown
import Html exposing (Html)
import Ui.Alert as Alert
import Ui.Layout as Layout


{-| The announcements, one under another; nothing when there are none.
-}
view : List Announcement -> Html msg
view announcements =
    if List.isEmpty announcements then
        Html.text ""

    else
        Layout.stack (List.map viewAnnouncement announcements)


viewAnnouncement : Announcement -> Html msg
viewAnnouncement announcement =
    let
        alert =
            case announcement.level of
                AnnouncementLevel.Info ->
                    Alert.info ""

                AnnouncementLevel.Warning ->
                    Alert.warning ""

                AnnouncementLevel.Critical ->
                    Alert.danger ""
    in
    Alert.toHtmlWithContent
        [ Markdown.view { headingOffset = 1 } announcement.content ]
        alert
