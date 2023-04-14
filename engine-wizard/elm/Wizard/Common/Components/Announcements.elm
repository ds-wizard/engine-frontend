module Wizard.Common.Components.Announcements exposing
    ( viewDashboard
    , viewLoginScreen
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Shared.Data.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement exposing (Announcement)
import Shared.Data.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement.AnnouncementLevel as AnnouncementLevel
import Shared.Html exposing (emptyNode)
import Shared.Markdown as Markdown
import Wizard.Common.Html.Attribute exposing (dataCy)


viewDashboard : List Announcement -> Html msg
viewDashboard =
    view << List.filter .dashboard


viewLoginScreen : List Announcement -> Html msg
viewLoginScreen =
    view << List.filter .loginScreen


view : List Announcement -> Html msg
view announcements =
    if List.isEmpty announcements then
        emptyNode

    else
        div [] (List.map viewAnnouncement announcements)


viewAnnouncement : Announcement -> Html msg
viewAnnouncement announcement =
    let
        alertClass =
            case announcement.level of
                AnnouncementLevel.Info ->
                    "info"

                AnnouncementLevel.Warning ->
                    "warning"

                AnnouncementLevel.Critical ->
                    "danger"
    in
    div [ class ("alert alert-" ++ alertClass), dataCy ("announcement_" ++ alertClass) ]
        [ Markdown.toHtml [] announcement.content ]
