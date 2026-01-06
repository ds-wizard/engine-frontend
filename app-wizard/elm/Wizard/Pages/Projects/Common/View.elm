module Wizard.Pages.Projects.Common.View exposing
    ( ProjectLike
    , shareIcon
    , shareTooltipHtml
    , visibilityIcon
    )

import Common.Components.FontAwesome exposing (faProjectSharingInternal, faProjectSharingPrivate, faProjectSharingPublic)
import Common.Components.Tooltip exposing (tooltipCustom)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, small, span, strong, text)
import Html.Attributes exposing (class)
import String.Format as String
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing exposing (ProjectSharing(..))
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility(..))
import Wizard.Data.AppState exposing (AppState)


visibilityIcon : AppState -> ProjectLike p -> Html msg
visibilityIcon appState project =
    span (tooltipCustom "with-tooltip-wide with-tooltip-right" <| shareTooltipString appState project)
        [ shareIcon project ]


shareIcon : ProjectLike p -> Html msg
shareIcon project =
    if isPublic project then
        faProjectSharingPublic

    else if isPrivate project then
        faProjectSharingPrivate

    else
        faProjectSharingInternal


shareTooltipHtml : AppState -> ProjectLike p -> List (Html msg)
shareTooltipHtml appState project =
    if isPublic project then
        [ small [ class "d-block fw-bold" ] [ text <| gettext "Public link" appState.locale ]
        , small [] [ text <| gettext "Anyone with the link can access the project. No login is required." appState.locale ]
        ]

    else if isPrivate project then
        [ small [] [ text <| gettext "Private project accessible only by you." appState.locale ] ]

    else if project.visibility /= ProjectVisibility.Private then
        [ small [ class "d-block fw-bold" ] [ text <| gettext "Visible by all other logged-in users" appState.locale ]
        , small [] [ text <| gettext "Other logged-in users can access the project. No explicit permission is required." appState.locale ]
        ]

    else
        let
            ( userCount, userGroupCount ) =
                memberCounts project
        in
        if userCount > 0 && userGroupCount > 0 then
            [ small [] <|
                String.formatHtml (gettext "Shared with %s and %s." appState.locale)
                    [ span [] <|
                        String.formatHtml (ngettext ( "%s user", "%s users" ) userCount appState.locale)
                            [ strong [] [ text (String.fromInt userCount) ] ]
                    , span [] <|
                        String.formatHtml (ngettext ( "%s user group", "%s user groups" ) userGroupCount appState.locale)
                            [ strong [] [ text (String.fromInt userGroupCount) ] ]
                    ]
            ]

        else if userGroupCount > 0 then
            [ small [] <|
                String.formatHtml (ngettext ( "Shared with %s user group.", "Shared with %s user groups." ) userGroupCount appState.locale)
                    [ strong [] [ text (String.fromInt userGroupCount) ]
                    ]
            ]

        else
            [ small [] <|
                String.formatHtml (ngettext ( "Shared with %s user.", "Shared with %s users." ) userCount appState.locale)
                    [ strong [] [ text (String.fromInt userCount) ]
                    ]
            ]


shareTooltipString : AppState -> ProjectLike p -> String
shareTooltipString appState project =
    if isPublic project then
        gettext "Anyone with the link can access the project." appState.locale

    else if isPrivate project then
        gettext "Private project accessible only by you." appState.locale

    else if project.visibility /= ProjectVisibility.Private then
        gettext "Other logged-in users can access the project." appState.locale

    else
        let
            ( userCount, userGroupCount ) =
                memberCounts project
        in
        if userCount > 0 && userGroupCount > 0 then
            String.format (gettext "Shared with %s and %s." appState.locale)
                [ String.format (ngettext ( "%s user", "%s users" ) userCount appState.locale) [ String.fromInt userCount ]
                , String.format (ngettext ( "%s user group", "%s user groups" ) userGroupCount appState.locale) [ String.fromInt userGroupCount ]
                ]

        else if userGroupCount > 0 then
            String.format (ngettext ( "Shared with %s user group.", "Shared with %s user groups." ) userGroupCount appState.locale)
                [ String.fromInt userGroupCount ]

        else
            String.format (ngettext ( "Shared with %s user.", "Shared with %s users." ) userCount appState.locale)
                [ String.fromInt userCount ]


type alias ProjectLike p =
    { p
        | permissions : List Permission
        , sharing : ProjectSharing
        , visibility : ProjectVisibility
    }


isPrivate : ProjectLike p -> Bool
isPrivate project =
    if project.sharing /= Restricted || project.visibility /= Private then
        False

    else
        case project.permissions of
            perm :: [] ->
                Member.isUserMember perm.member

            _ ->
                False


isPublic : ProjectLike p -> Bool
isPublic project =
    project.sharing /= Restricted


memberCounts : ProjectLike p -> ( Int, Int )
memberCounts project =
    project.permissions
        |> List.map .member
        |> List.partition Member.isUserMember
        |> Tuple.mapBoth List.length List.length
