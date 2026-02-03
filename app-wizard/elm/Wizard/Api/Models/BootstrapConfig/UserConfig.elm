module Wizard.Api.Models.BootstrapConfig.UserConfig exposing
    ( UserConfig
    , decoder
    , toUserSuggestion
    )

import Common.Api.Models.UserSuggestion exposing (UserSuggestion)
import Common.Data.Role as Role exposing (Role)
import Dict exposing (Dict)
import Gravatar
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias UserConfig =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : Role
    , permissions : List String
    , imageUrl : Maybe String
    , userGroupUuids : List Uuid
    , lastSeenNewsId : Maybe String
    , pluginSettings : Dict String String
    }


decoder : Decoder UserConfig
decoder =
    D.succeed UserConfig
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "role" Role.decoder
        |> D.required "permissions" (D.list D.string)
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "userGroupUuids" (D.list Uuid.decoder)
        |> D.required "lastSeenNewsId" (D.maybe D.string)
        |> D.required "pluginSettings" (D.dict D.valueAsString)


toUserSuggestion : UserConfig -> UserSuggestion
toUserSuggestion userInfo =
    { uuid = userInfo.uuid
    , firstName = userInfo.firstName
    , lastName = userInfo.lastName
    , gravatarHash = Gravatar.hashEmail userInfo.email
    , imageUrl = userInfo.imageUrl
    }
