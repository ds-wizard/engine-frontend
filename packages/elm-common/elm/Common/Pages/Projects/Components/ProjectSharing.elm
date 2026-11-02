module Common.Pages.Projects.Components.ProjectSharing exposing (description, icon, view)

{-| Who can reach a project — public link, only the owner, every logged-in
user, or the people and groups it is shared with — as an icon with the
sentence that explains it.

The sentence is the icon's accessible name and its tooltip, so it is read with
the row by a screen reader and shown on hover.

`icon` and `description` are the two halves on their own, for the project's own
page: the icon goes on its Share button and the sentence under the actions that
change it.

-}

import Common.Api.Models.Member as Member
import Common.Api.Models.Project.ProjectSharing exposing (ProjectSharing(..))
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility(..))
import Common.Data.AppState exposing (AppState)
import Common.Data.ProjectUtils exposing (ProjectLike)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, span)
import Html.Attributes exposing (class)
import String.Format as String
import Ui.Icon as Icon exposing (Icon)
import Ui.Tooltip as Tooltip


view : AppState -> ProjectLike p -> Html msg
view appState project =
    let
        label =
            description appState project
    in
    Tooltip.view { label = label, placement = Tooltip.Below }
        (span [ class "c-project-sharing" ]
            [ Icon.toHtml (Icon.labeled label (icon project)) ]
        )


{-| Which of the three states the project is in — public, private, or shared
with someone.
-}
icon : ProjectLike p -> Icon
icon project =
    if isPublic project then
        Icon.solid "fa-globe"

    else if isPrivate project then
        Icon.solid "fa-lock"

    else
        Icon.solid "fa-user-group"


{-| Who can reach the project, as one sentence.
-}
description : AppState -> ProjectLike p -> String
description appState project =
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
