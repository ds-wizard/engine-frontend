module Common.UserGroupsFeatureTest exposing (userGroupsFeatureTests)

import Common.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.BootstrapConfig.UserConfig exposing (UserConfig)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Api.Models.UserGroupMember exposing (UserGroupMember)
import Common.Api.Models.UserGroupMembershipType exposing (UserGroupMembershipType(..))
import Common.Data.Feature as Feature
import Common.Data.Session as Session exposing (Session)
import Common.Data.WizardRolePermission as WizardRolePermission
import Dict
import Expect
import Test exposing (Test, describe, test)
import Uuid exposing (Uuid)


ada : Uuid
ada =
    Uuid.fromUuidString "11111111-1111-4111-8111-111111111111"


bob : Uuid
bob =
    Uuid.fromUuidString "22222222-2222-4222-8222-222222222222"


user : List RolePermission -> UserConfig
user permissions =
    { uuid = ada
    , email = "ada@example.com"
    , firstName = "Ada"
    , lastName = "Admin"
    , organizationRole = { uuid = ada, name = "Member", permissions = permissions }
    , workspaceRoles = Dict.empty
    , permissions = permissions
    , workspaceRoleHere = Nothing
    , imageUrl = Nothing
    , userGroupUuids = []
    , lastSeenNewsId = Nothing
    , pluginSettings = Dict.empty
    , affiliation = Nothing
    }


appState : Maybe UserConfig -> { config : BootstrapConfig, session : Session }
appState mbUser =
    let
        config =
            BootstrapConfig.default
    in
    { config = { config | user = mbUser }, session = Session.init "" }


member : Uuid -> UserGroupMembershipType -> UserGroupMember
member uuid membershipType =
    { uuid = uuid
    , firstName = ""
    , lastName = ""
    , gravatarHash = ""
    , imageUrl = Nothing
    , membershipType = membershipType
    , affiliation = Nothing
    }


userGroupsFeatureTests : Test
userGroupsFeatureTests =
    describe "user groups"
        [ test "anybody signed in sees the groups" <|
            \_ ->
                Feature.userGroupsView (appState (Just (user [])))
                    |> Expect.equal True
        , test "nobody signed out does" <|
            \_ ->
                Feature.userGroupsView (appState Nothing)
                    |> Expect.equal False
        , test "creating one takes Create User Groups" <|
            \_ ->
                ( Feature.userGroupsCreate (appState (Just (user [ WizardRolePermission.userGroupsCreate ])))
                , Feature.userGroupsCreate (appState (Just (user [])))
                )
                    |> Expect.equal ( True, False )
        , test "its owner changes a group" <|
            \_ ->
                Feature.userGroupEdit (appState (Just (user []))) { users = [ member ada Owner ] }
                    |> Expect.equal True
        , test "a member of it does not" <|
            \_ ->
                Feature.userGroupEdit (appState (Just (user []))) { users = [ member ada Member, member bob Owner ] }
                    |> Expect.equal False
        , test "Manage User Groups changes any group" <|
            \_ ->
                Feature.userGroupEdit (appState (Just (user [ WizardRolePermission.userGroupsManage ]))) { users = [ member bob Owner ] }
                    |> Expect.equal True
        ]
