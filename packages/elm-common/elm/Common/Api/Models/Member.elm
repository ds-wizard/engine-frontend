module Common.Api.Models.Member exposing
    ( Member
    , compare
    , decoder
    , getUuid
    , isUserMember
    , toQuestionnaireEditFormMemberType
    , toUserGroupSuggestion
    , toUserSuggestion
    , userGroupMember
    , userMember
    , visibleName
    )

import Common.Api.Models.User as User
import Common.Api.Models.UserGroupSuggestion as UserGroupSuggestion exposing (UserGroupSuggestion)
import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Common.Data.Forms.ProjectShareFormMemberType exposing (ProjectShareFormMemberType(..))
import Json.Decode as D exposing (Decoder)
import Uuid exposing (Uuid)


type Member
    = UserMember UserSuggestion
    | UserGroupMember UserGroupSuggestion


userMember : UserSuggestion -> Member
userMember =
    UserMember


userGroupMember : UserGroupSuggestion -> Member
userGroupMember =
    UserGroupMember


isUserMember : Member -> Bool
isUserMember member =
    case member of
        UserMember _ ->
            True

        UserGroupMember _ ->
            False


getUuid : Member -> Uuid
getUuid member =
    case member of
        UserMember data ->
            data.uuid

        UserGroupMember data ->
            data.uuid


visibleName : Member -> String
visibleName member =
    case member of
        UserMember data ->
            User.fullName data

        UserGroupMember data ->
            data.name


compare : Member -> Member -> Order
compare member1 member2 =
    case ( member1, member2 ) of
        ( UserMember _, UserGroupMember _ ) ->
            LT

        ( UserGroupMember _, UserMember _ ) ->
            GT

        ( UserMember data1, UserMember data2 ) ->
            User.compare data1 data2

        ( UserGroupMember data1, UserGroupMember data2 ) ->
            Basics.compare data1.name data2.name


decoder : Decoder Member
decoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "UserMember" ->
                        D.map UserMember UserSuggestion.decoder

                    "UserGroupMember" ->
                        D.map UserGroupMember UserGroupSuggestion.decoder

                    _ ->
                        D.fail <| "Unknown member type " ++ type_
            )


toUserSuggestion : Member -> Maybe UserSuggestion
toUserSuggestion member =
    case member of
        UserMember userSuggestion ->
            Just userSuggestion

        _ ->
            Nothing


toUserGroupSuggestion : Member -> Maybe UserGroupSuggestion
toUserGroupSuggestion member =
    case member of
        UserGroupMember userGroupSuggestion ->
            Just userGroupSuggestion

        _ ->
            Nothing


toQuestionnaireEditFormMemberType : Member -> ProjectShareFormMemberType
toQuestionnaireEditFormMemberType member =
    case member of
        UserMember _ ->
            UserProjectPermType

        UserGroupMember _ ->
            UserGroupProjectPermType
