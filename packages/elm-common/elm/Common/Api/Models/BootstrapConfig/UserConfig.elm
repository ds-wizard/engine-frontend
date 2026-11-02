module Common.Api.Models.BootstrapConfig.UserConfig exposing
    ( UserConfig
    , decoder
    , encode
    , hasPerm
    , permissionsIn
    , toUserSuggestion
    , withPermissions
    , withWorkspaceRoleHere
    , workspaceRole
    )

{-| The person signed in.

They have one role in the organization and may have one in each workspace
(`workspaceRoles`, by the workspace's uuid). What they may do in a workspace is
what either role allows; a permission of the organization role holds in every
workspace.

`permissions` is what is in force where they are now, and `workspaceRoleHere` the
role of the workspace they are in. It starts as the
organization role's, and an application that has workspaces sets it as the user
moves (`permissionsIn`, `withPermissions`); every check reads it (`hasPerm`).

-}

import Common.Api.Models.RoleInfo as RoleInfo exposing (RoleInfo)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Dict exposing (Dict)
import Gravatar
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import List.Extra as List
import Uuid exposing (Uuid)


type alias UserConfig =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , organizationRole : RoleInfo
    , workspaceRoles : Dict String RoleInfo
    , permissions : List RolePermission
    , workspaceRoleHere : Maybe RoleInfo
    , imageUrl : Maybe String
    , userGroupUuids : List Uuid
    , lastSeenNewsId : Maybe String
    , pluginSettings : Dict String String
    , affiliation : Maybe String
    }


decoder : Decoder UserConfig
decoder =
    D.field "organizationRole" RoleInfo.decoder
        |> D.andThen decoderWith


decoderWith : RoleInfo -> Decoder UserConfig
decoderWith organizationRole =
    D.succeed UserConfig
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.hardcoded organizationRole
        |> D.required "workspaceRoles" (D.dict RoleInfo.decoder)
        |> D.hardcoded organizationRole.permissions
        |> D.hardcoded Nothing
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "userGroupUuids" (D.list Uuid.decoder)
        |> D.required "lastSeenNewsId" (D.maybe D.string)
        |> D.required "pluginSettings" (D.dict D.valueAsString)
        |> D.optional "affiliation" (D.maybe D.string) Nothing


{-| The user as a plugin is given them: the role in the organization, and the
role in the workspace the user is in, if any (`workspaceRoleHere`).
-}
encode : UserConfig -> E.Value
encode userConfig =
    E.object
        [ ( "uuid", Uuid.encode userConfig.uuid )
        , ( "email", E.string userConfig.email )
        , ( "firstName", E.string userConfig.firstName )
        , ( "lastName", E.string userConfig.lastName )
        , ( "tenantRole", RoleInfo.encode userConfig.organizationRole )
        , ( "workspaceRole", E.maybe RoleInfo.encode userConfig.workspaceRoleHere )
        , ( "imageUrl", E.maybe E.string userConfig.imageUrl )
        , ( "affiliation", E.maybe E.string userConfig.affiliation )
        ]


toUserSuggestion : UserConfig -> UserSuggestion
toUserSuggestion userInfo =
    { uuid = userInfo.uuid
    , firstName = userInfo.firstName
    , lastName = userInfo.lastName
    , gravatarHash = Gravatar.hashEmail userInfo.email
    , imageUrl = userInfo.imageUrl
    , affiliation = userInfo.affiliation
    }


hasPerm : RolePermission -> UserConfig -> Bool
hasPerm perm user =
    List.member perm user.permissions


{-| The role the user has in a workspace of its own, if any.
-}
workspaceRole : Uuid -> UserConfig -> Maybe RoleInfo
workspaceRole workspaceUuid user =
    Dict.get (Uuid.toString workspaceUuid) user.workspaceRoles


{-| What the user may do in a workspace: what the organization role allows and
what the role in that workspace adds. Outside any workspace it is the
organization role's alone.
-}
permissionsIn : Maybe Uuid -> UserConfig -> List RolePermission
permissionsIn mbWorkspaceUuid user =
    let
        ofWorkspace =
            mbWorkspaceUuid
                |> Maybe.andThen (\workspaceUuid -> workspaceRole workspaceUuid user)
                |> Maybe.map .permissions
                |> Maybe.withDefault []
    in
    user.organizationRole.permissions ++ List.filter (\perm -> List.notMember perm user.organizationRole.permissions) ofWorkspace


withPermissions : List RolePermission -> UserConfig -> UserConfig
withPermissions permissions user =
    { user | permissions = permissions }


{-| The role of the workspace the user is in, which an application that has
workspaces sets as the user moves, as it sets the permissions.
-}
withWorkspaceRoleHere : Maybe RoleInfo -> UserConfig -> UserConfig
withWorkspaceRoleHere role user =
    { user | workspaceRoleHere = role }
