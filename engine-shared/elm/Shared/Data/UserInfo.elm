module Shared.Data.UserInfo exposing
    ( UserInfo
    , decoder
    , isAdmin
    , toUserSuggestion
    )

import Gravatar
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Auth.Role as Role
import Shared.Data.UserSuggestion exposing (UserSuggestion)
import Uuid exposing (Uuid)


type alias UserInfo =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : String
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
        |> D.required "role" D.string
        |> D.required "permissions" (D.list D.string)
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "userGroupUuids" (D.list Uuid.decoder)


isAdmin : Maybe UserInfo -> Bool
isAdmin =
    Maybe.map (.role >> (==) Role.admin) >> Maybe.withDefault False


toUserSuggestion : UserInfo -> UserSuggestion
toUserSuggestion userInfo =
    { uuid = userInfo.uuid
    , firstName = userInfo.firstName
    , lastName = userInfo.lastName
    , gravatarHash = Gravatar.hashEmail userInfo.email
    , imageUrl = userInfo.imageUrl
    }
