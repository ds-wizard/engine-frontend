module Common.Api.Models.UserInfo exposing
    ( UserInfo
    , decoder
    , fullName
    , isAdmin
    , isDataSteward
    , toUserSuggestion
    )

import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Data.Role as Role exposing (Role)
import Gravatar
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


type alias UserInfo =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : Role
    , permissions : List String
    , imageUrl : Maybe String
    , userGroupUuids : List Uuid
    }


decoder : Decoder UserInfo
decoder =
    D.succeed UserInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "role" Role.decoder
        |> D.required "permissions" (D.list D.string)
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.optional "userGroupUuids" (D.list Uuid.decoder) []


fullName : { a | firstName : String, lastName : String } -> String
fullName userInfo =
    userInfo.firstName ++ " " ++ userInfo.lastName


isAdmin : Maybe UserInfo -> Bool
isAdmin =
    Maybe.unwrap False (Role.isAdmin << .role)


isDataSteward : Maybe UserInfo -> Bool
isDataSteward =
    Maybe.unwrap False (Role.isDataSteward << .role)


toUserSuggestion : UserInfo -> UserSuggestion
toUserSuggestion userInfo =
    { uuid = userInfo.uuid
    , firstName = userInfo.firstName
    , lastName = userInfo.lastName
    , gravatarHash = Gravatar.hashEmail userInfo.email
    , imageUrl = userInfo.imageUrl
    }
