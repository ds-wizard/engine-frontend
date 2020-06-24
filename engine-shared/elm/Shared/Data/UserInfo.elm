module Shared.Data.UserInfo exposing
    ( UserInfo
    , decoder
    , encode
    , isAdmin
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Auth.Role as Role
import Uuid exposing (Uuid)


type alias UserInfo =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : String
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
        |> D.required "role" D.string
        |> D.required "permissions" (D.list D.string)
        |> D.required "imageUrl" (D.maybe D.string)


encode : UserInfo -> E.Value
encode userInfo =
    E.object
        [ ( "uuid", Uuid.encode userInfo.uuid )
        , ( "email", E.string userInfo.email )
        , ( "firstName", E.string userInfo.firstName )
        , ( "lastName", E.string userInfo.lastName )
        , ( "role", E.string userInfo.role )
        , ( "permissions", E.list E.string userInfo.permissions )
        , ( "imageUrl", E.maybe E.string userInfo.imageUrl )
        ]


isAdmin : Maybe UserInfo -> Bool
isAdmin =
    Maybe.map (.role >> (==) Role.admin) >> Maybe.withDefault False
