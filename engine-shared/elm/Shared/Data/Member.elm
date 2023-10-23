module Shared.Data.Member exposing
    ( Member
    , compare
    , decoder
    , getUuid
    , imageUrl
    , toQuestionnaireEditFormMemberType
    , toUserGroupSuggestion
    , toUserSuggestion
    , userMember
    , visibleName
    )

import Json.Decode as D exposing (Decoder)
import Shared.Data.User as User
import Shared.Data.UserGroupSuggestion as UserGroupSuggestion exposing (UserGroupSuggestion)
import Shared.Data.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Uuid exposing (Uuid)
import Wizard.Projects.Common.QuestionnaireEditFormMemberType exposing (QuestionnaireEditFormMemberType(..))


type Member
    = UserMember UserSuggestion
    | UserGroupMember UserGroupSuggestion


userMember : UserSuggestion -> Member
userMember =
    UserMember


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


imageUrl : Member -> Maybe String
imageUrl member =
    case member of
        UserMember data ->
            Just (User.imageUrlOrGravatar data)

        UserGroupMember _ ->
            Nothing


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


toQuestionnaireEditFormMemberType : Member -> QuestionnaireEditFormMemberType
toQuestionnaireEditFormMemberType member =
    case member of
        UserMember _ ->
            UserQuestionnairePermType

        UserGroupMember _ ->
            UserGroupQuestionnairePermType
