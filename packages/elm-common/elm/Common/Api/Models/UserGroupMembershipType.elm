module Common.Api.Models.UserGroupMembershipType exposing
    ( UserGroupMembershipType(..)
    , decoder
    , encode
    , fromString
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type UserGroupMembershipType
    = Owner
    | Member


decoder : Decoder UserGroupMembershipType
decoder =
    D.string
        |> D.andThen
            (\membershipType ->
                case fromString membershipType of
                    Just value ->
                        D.succeed value

                    Nothing ->
                        D.fail ("Unexpected user group membership type " ++ membershipType)
            )


encode : UserGroupMembershipType -> E.Value
encode =
    E.string << toString


fromString : String -> Maybe UserGroupMembershipType
fromString str =
    case str of
        "OwnerUserGroupMembershipType" ->
            Just Owner

        "MemberUserGroupMembershipType" ->
            Just Member

        _ ->
            Nothing


toString : UserGroupMembershipType -> String
toString membershipType =
    case membershipType of
        Owner ->
            "OwnerUserGroupMembershipType"

        Member ->
            "MemberUserGroupMembershipType"
