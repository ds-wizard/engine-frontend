module Wizard.Api.Models.Project exposing
    ( Project
    , decoder
    , isEditable
    , isOwner
    )

import Common.Api.Models.UserInfo as UserInfo
import Flip exposing (flip)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import List.Extra as List
import Time
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelPackageInfo as KnowledgeModelPackageInfo exposing (KnowledgeModelPackageInfo)
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission as Permission exposing (Permission)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing(..))
import Wizard.Api.Models.Project.ProjectState as ProjectState exposing (ProjectState)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility(..))
import Wizard.Api.Models.ProjectPerm as ProjectPerm
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session


type alias Project =
    { uuid : Uuid
    , name : String
    , description : Maybe String
    , isTemplate : Bool
    , knowledgeModelPackage : KnowledgeModelPackageInfo
    , visibility : ProjectVisibility
    , sharing : ProjectSharing
    , permissions : List Permission
    , state : ProjectState
    , updatedAt : Time.Posix
    }


isEditable : AppState -> Project -> Bool
isEditable appState project =
    let
        isAdmin =
            UserInfo.isAdmin appState.config.user

        isReadonly =
            if project.sharing == AnyoneWithLinkEdit then
                False

            else if Session.exists appState.session then
                project.visibility == VisibleView || (project.visibility == Private && not isMember)

            else
                project.sharing == AnyoneWithLinkView

        isMember =
            matchMember project appState.config.user
    in
    isAdmin || not isReadonly || isMember


isOwner : AppState -> Project -> Bool
isOwner appState project =
    let
        isAdmin =
            UserInfo.isAdmin appState.config.user

        isQuestionnaireOwner =
            appState.config.user
                |> Maybe.andThen (\user -> List.find (\p -> Member.getUuid p.member == user.uuid) project.permissions)
                |> Maybe.map (List.member ProjectPerm.admin << .perms)
                |> Maybe.withDefault False
    in
    isAdmin || isQuestionnaireOwner


decoder : Decoder Project
decoder =
    D.succeed Project
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "isTemplate" D.bool
        |> D.required "knowledgeModelPackage" KnowledgeModelPackageInfo.decoder
        |> D.required "visibility" ProjectVisibility.decoder
        |> D.required "sharing" ProjectSharing.decoder
        |> D.required "permissions" (D.list Permission.decoder)
        |> D.required "state" ProjectState.decoder
        |> D.required "updatedAt" D.datetime


matchMember : Project -> Maybe { a | uuid : Uuid, userGroupUuids : List Uuid } -> Bool
matchMember project mbUser =
    case mbUser of
        Just user ->
            let
                userUuids =
                    user.uuid :: user.userGroupUuids

                memberUuids =
                    List.map (Member.getUuid << .member) project.permissions
            in
            List.any (flip List.member memberUuids) userUuids

        Nothing ->
            False
