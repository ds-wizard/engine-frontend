module Registry.Components.SidebarRow exposing (ViewIdProps, ViewOtherVersionsProps, ViewProps, view, viewId, viewLicense, viewMetamodelVersion, viewOrganization, viewOtherVersions, viewPublishedOn, viewVersion)

import Gettext exposing (gettext)
import Html exposing (Html, a, div, h3, img, strong, text)
import Html.Attributes exposing (class, href, src, target)
import Html.Extra as Html
import Registry.Api.Models.OrganizationInfo exposing (OrganizationInfo)
import Registry.Components.ItemIdBox as ItemIdBox
import Registry.Components.VersionList as VersionList
import Registry.Data.AppState exposing (AppState)
import Shared.Common.TimeUtils as TimeUtils
import Time
import Version exposing (Version)


type alias ViewProps msg =
    { title : String
    , content : List (Html msg)
    }


view : ViewProps msg -> Html msg
view props =
    div [ class "sidebar-row" ]
        [ h3 [ class "fs-sm text-secondary" ] [ text props.title ]
        , div [] props.content
        ]


type alias ViewIdProps msg =
    { title : String
    , id : String
    , wrapMsg : ItemIdBox.Msg -> msg
    , itemIdBoxState : ItemIdBox.State
    }


viewId : AppState -> ViewIdProps msg -> Html msg
viewId appState props =
    view
        { title = props.title
        , content =
            [ Html.map props.wrapMsg <|
                ItemIdBox.view appState props.itemIdBoxState { id = props.id }
            ]
        }


viewLicense : AppState -> String -> Html msg
viewLicense appState license =
    view
        { title = gettext "License Model ID" appState.locale
        , content =
            [ a [ href ("https://spdx.org/licenses/" ++ license ++ ".html"), target "_blank" ]
                [ text license ]
            ]
        }


viewVersion : AppState -> Version -> Html msg
viewVersion appState version =
    view
        { title = gettext "Version" appState.locale
        , content =
            [ text (Version.toString version) ]
        }


type alias ViewOtherVersionsProps msg =
    { versions : List Version
    , currentVersion : Version
    , toUrl : Version -> String
    , wrapMsg : VersionList.Msg -> msg
    , versionListState : VersionList.State
    }


viewOtherVersions : AppState -> ViewOtherVersionsProps msg -> Html msg
viewOtherVersions appState props =
    Html.viewIf (List.length props.versions > 1) <|
        view
            { title = gettext "Other Versions" appState.locale
            , content =
                [ Html.map props.wrapMsg <|
                    VersionList.view props.versionListState
                        { versions = props.versions
                        , currentVersion = props.currentVersion
                        , toUrl = props.toUrl
                        }
                ]
            }


viewMetamodelVersion : AppState -> Int -> Html msg
viewMetamodelVersion appState metamodelVersion =
    view
        { title = gettext "Metamodel Version" appState.locale
        , content =
            [ text (String.fromInt metamodelVersion) ]
        }


viewPublishedOn : AppState -> Time.Posix -> Html msg
viewPublishedOn appState published =
    view
        { title = gettext "Published on" appState.locale
        , content =
            [ text (TimeUtils.toReadableDate appState.timeZone published) ]
        }


viewOrganization : AppState -> OrganizationInfo -> Html msg
viewOrganization appState organization =
    let
        logo =
            case organization.logo of
                Just url ->
                    img [ src url, class "rounded-circle organization-icon me-2" ] []

                Nothing ->
                    Html.nothing
    in
    view
        { title = gettext "Published by" appState.locale
        , content =
            [ div [ class "organization d-flex align-items-center" ]
                [ logo
                , div []
                    [ strong [] [ text organization.name ]
                    , div [ class "font-monospace fs-sm" ] [ text organization.organizationId ]
                    ]
                ]
            ]
        }
