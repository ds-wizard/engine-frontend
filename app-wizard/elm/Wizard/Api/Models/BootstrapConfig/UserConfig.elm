module Wizard.Api.Models.BootstrapConfig.UserConfig exposing
    ( UserConfig
    , decoder
    , encode
    , hasPerm
    , toUserSuggestion
    )

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
import Uuid exposing (Uuid)


type alias UserConfig =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : RoleInfo
    , imageUrl : Maybe String
    , userGroupUuids : List Uuid
    , lastSeenNewsId : Maybe String
    , pluginSettings : Dict String String
    , affiliation : Maybe String
    }


decoder : Decoder UserConfig
decoder =
    D.succeed UserConfig
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "role" RoleInfo.decoder
        |> D.required "imageUrl" (D.maybe D.string)
        |> D.required "userGroupUuids" (D.list Uuid.decoder)
        |> D.required "lastSeenNewsId" (D.maybe D.string)
        |> D.required "pluginSettings" (D.dict D.valueAsString)
        |> D.optional "affiliation" (D.maybe D.string) Nothing


encode : UserConfig -> E.Value
encode userConfig =
    E.object
        [ ( "uuid", Uuid.encode userConfig.uuid )
        , ( "email", E.string userConfig.email )
        , ( "firstName", E.string userConfig.firstName )
        , ( "lastName", E.string userConfig.lastName )
        , ( "role", RoleInfo.encode userConfig.role )
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
    List.member perm user.role.permissions
