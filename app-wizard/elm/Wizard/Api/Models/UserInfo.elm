module Wizard.Api.Models.UserInfo exposing
    ( UserInfo
    , decoder
    , isAdmin
    , isDataSteward
    , toUserSuggestion
    )

import Gravatar
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe
import Shared.Data.Role as Role exposing (Role)
import Uuid exposing (Uuid)
import Wizard.Api.Models.UserSuggestion exposing (UserSuggestion)


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
        |> D.required "userGroupUuids" (D.list Uuid.decoder)


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
