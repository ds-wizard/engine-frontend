module Wizard.Utils.ProjectUtils exposing
    ( ProjectLike
    , canComment
    , isAnonymousProject
    , isEditor
    , isMigrating
    , isOwner
    )

import Common.Api.Models.UserInfo as UserInfo
import Flip exposing (flip)
import List.Extra as List
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing exposing (ProjectSharing(..))
import Wizard.Api.Models.Project.ProjectVisibility exposing (ProjectVisibility(..))
import Wizard.Api.Models.ProjectPerm as ProjectPerm
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session


type alias ProjectLike q =
    { q
        | permissions : List Permission
        , sharing : ProjectSharing
        , visibility : ProjectVisibility
        , migrationUuid : Maybe Uuid
    }


isMigrating : { q | migrationUuid : Maybe Uuid } -> Bool
isMigrating =
    Maybe.isJust << .migrationUuid


isEditor : AppState -> ProjectLike q -> Bool
isEditor appState project =
    hasPerm appState project ProjectPerm.edit


isOwner : AppState -> ProjectLike q -> Bool
isOwner appState project =
    hasPerm appState project ProjectPerm.admin


isAnonymousProject : ProjectLike q -> Bool
isAnonymousProject project =
    List.isEmpty project.permissions


canComment : AppState -> ProjectLike q -> Bool
canComment appState project =
    hasPerm appState project ProjectPerm.comment && not (isMigrating project)


hasPerm : AppState -> ProjectLike q -> String -> Bool
hasPerm appState project role =
    let
        mbUser =
            appState.config.user

        isAuthenticated =
            Session.exists appState.session

        globalPerms =
            if UserInfo.isAdmin mbUser then
                ProjectPerm.all

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
