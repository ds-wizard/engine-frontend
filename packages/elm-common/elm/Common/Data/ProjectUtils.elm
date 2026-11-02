module Common.Data.ProjectUtils exposing
    ( ProjectLike
    , canComment
    , isAnonymousProject
    , isEditor
    , isOwner
    )

import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Api.Models.Member as Member
import Common.Api.Models.Permission exposing (Permission)
import Common.Api.Models.Project.ProjectSharing exposing (ProjectSharing(..))
import Common.Api.Models.Project.ProjectVisibility exposing (ProjectVisibility(..))
import Common.Api.Models.ProjectPerm as ProjectPerm
import Common.Data.Session as Session exposing (Session)
import Common.Data.WizardRolePermission as RolePermission
import Flip exposing (flip)
import List.Extra as List
import Maybe.Extra as Maybe


type alias ProjectLike q =
    { q
        | permissions : List Permission
        , sharing : ProjectSharing
        , visibility : ProjectVisibility
    }


isEditor : { a | config : BootstrapConfig, session : Session } -> ProjectLike q -> Bool
isEditor appState project =
    hasPerm appState project ProjectPerm.edit


isOwner : { a | config : BootstrapConfig, session : Session } -> ProjectLike q -> Bool
isOwner appState project =
    hasPerm appState project ProjectPerm.admin


isAnonymousProject : ProjectLike q -> Bool
isAnonymousProject project =
    List.isEmpty project.permissions


canComment : { a | config : BootstrapConfig, session : Session } -> ProjectLike q -> Bool
canComment appState project =
    hasPerm appState project ProjectPerm.comment


hasPerm : { a | config : BootstrapConfig, session : Session } -> ProjectLike q -> String -> Bool
hasPerm appState project role =
    let
        mbUser =
            appState.config.user

        isAuthenticated =
            Session.exists appState.session

        hasRolePerm perm =
            Maybe.unwrap False (UserConfig.hasPerm perm) appState.config.user

        globalPerms =
            if hasRolePerm RolePermission.projectsManage then
                ProjectPerm.all

            else if hasRolePerm RolePermission.projectsEdit then
                [ ProjectPerm.view, ProjectPerm.comment, ProjectPerm.edit ]

            else if hasRolePerm RolePermission.projectsComment then
                [ ProjectPerm.view, ProjectPerm.comment ]

            else if hasRolePerm RolePermission.projectsView then
                [ ProjectPerm.view ]

            else
                []

        visibilityPerms =
            if isAuthenticated then
                case project.visibility of
                    VisibleEdit ->
                        [ ProjectPerm.view, ProjectPerm.comment, ProjectPerm.edit ]

                    VisibleComment ->
                        [ ProjectPerm.view, ProjectPerm.comment ]

                    VisibleView ->
                        [ ProjectPerm.view ]

                    Private ->
                        []

            else
                []

        sharingPerms =
            case project.sharing of
                AnyoneWithLinkEdit ->
                    [ ProjectPerm.view, ProjectPerm.comment, ProjectPerm.edit ]

                AnyoneWithLinkComment ->
                    [ ProjectPerm.view, ProjectPerm.comment ]

                AnyoneWithLinkView ->
                    [ ProjectPerm.view ]

                Restricted ->
                    []

        memberPerms =
            case mbUser of
                Just user ->
                    let
                        userUuids =
                            user.uuid :: user.userGroupUuids
                    in
                    project.permissions
                        |> List.filter (flip List.member userUuids << Member.getUuid << .member)
                        |> List.concatMap .perms

                Nothing ->
                    []

        appliedPerms =
            List.unique <| globalPerms ++ visibilityPerms ++ sharingPerms ++ memberPerms
    in
    List.member role appliedPerms
