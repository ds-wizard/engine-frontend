module Common.Api.Models.UserInfo exposing
    ( UserInfo
    , decoder
    , fullName
    , isAdmin
    , isDataSteward
    )

import Common.Data.Role as Role exposing (Role)
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


fullName : { a | firstName : String, lastName : String } -> String
fullName userInfo =
    userInfo.firstName ++ " " ++ userInfo.lastName


isAdmin : Maybe { a | role : Role } -> Bool
isAdmin =
    Maybe.unwrap False (Role.isAdmin << .role)


isDataSteward : Maybe { a | role : Role } -> Bool
isDataSteward =
    Maybe.unwrap False (Role.isDataSteward << .role)
