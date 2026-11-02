module Common.Api.Models.UserGroupMember exposing
    ( UserGroupMember
    , decoder
    , encode
    , fromUserSuggestion
    )

import Common.Api.Models.UserGroupMembershipType as UserGroupMembershipType exposing (UserGroupMembershipType)
import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias UserGroupMember =
    { uuid : Uuid
    , firstName : String
    , lastName : String
    , gravatarHash : String
    , imageUrl : Maybe String
    , membershipType : UserGroupMembershipType
    , affiliation : Maybe String
    }


decoder : Decoder UserGroupMember
decoder =
    D.succeed UserGroupMember
        |> D.required "uuid" Uuid.decoder
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "gravatarHash" D.string
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "membershipType" UserGroupMembershipType.decoder
        |> D.required "affiliation" (D.maybe D.string)


{-| One row of `PUT /user-groups/{uuid}/memberships`: who, and as what.
-}
encode : UserGroupMember -> E.Value
encode member =
    E.object
        [ ( "mType", UserGroupMembershipType.encode member.membershipType )
        , ( "userUuid", Uuid.encode member.uuid )
        ]


fromUserSuggestion : UserSuggestion -> UserGroupMember
fromUserSuggestion user =
    { uuid = user.uuid
    , firstName = user.firstName
    , lastName = user.lastName
    , gravatarHash = user.gravatarHash
    , imageUrl = user.imageUrl
    , membershipType = UserGroupMembershipType.Member
    , affiliation = user.affiliation
    }
