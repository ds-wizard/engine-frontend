module Registry2.Components.ListItem exposing (Item, ViewConfig, view)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, img, p, span, text)
import Html.Attributes exposing (class, href, src)
import Html.Extra as Html
import Registry2.Api.Models.OrganizationInfo exposing (OrganizationInfo)
import Registry2.Data.AppState exposing (AppState)
import Registry2.Routes as Routes
import Shared.Common.TimeUtils as TimeUtils
import String.Format as String
import Time
import Version exposing (Version)


type alias Item a =
    { a
        | createdAt : Time.Posix
        , description : String
        , id : String
        , name : String
        , organization : OrganizationInfo
        , version : Version
    }


type alias ViewConfig a =
    { toRoute : Item a -> Routes.Route }


view : AppState -> ViewConfig a -> Item a -> Html msg
view appState config item =
    let
        logo =
            case item.organization.logo of
                Just url ->
                    img [ src url, class "organization-icon rounded-circle" ] []

                Nothing ->
                    Html.nothing
    in
    Html.div [ class "list-item shadow-sm rounded px-4 py-3" ]
        [ div [ class "d-flex justify-content-start align-items-center" ]
            [ a
                [ href (Routes.toUrl (config.toRoute item))
                , class "list-item-link"
                ]
                [ text item.name ]
            , span [ class "badge text-bg-light ms-2" ] [ text (Version.toString item.version) ]
            ]
        , p [ class "text-muted mt-2" ] [ Html.text item.description ]
        , div [ class "publish-info publish-info-bt font-monospace" ]
            [ span [ class "fragment" ]
                (String.formatHtml (gettext "Published by %s" appState.locale)
                    [ span [ class "organization" ]
                        [ logo
                        , text item.organization.name
                        ]
                    ]
                )
            , span
                [ class "fragment" ]
                [ text (TimeUtils.toReadableDate appState.timeZone item.createdAt) ]
            ]
        ]
