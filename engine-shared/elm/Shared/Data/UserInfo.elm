module Shared.Data.UserInfo exposing
    ( UserInfo
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type UserInfo
    = UserInfo Internals


type alias Internals =
    { uuid : Uuid
    , email : String
    , firstName : String
    , lastName : String
    , role : String
    , permissions : List String
    }


decoder : Decoder UserInfo
decoder =
    D.succeed Internals
        |> D.required "uuid" Uuid.decoder
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "role" D.string
        |> D.required "permissions" (D.list D.string)
        |> D.map UserInfo


encode : UserInfo -> E.Value
encode (UserInfo userInfo) =
    E.object
        [ ( "uuid", Uuid.encode userInfo.uuid )
        , ( "email", E.string userInfo.email )
        , ( "firstName", E.string userInfo.firstName )
        , ( "lastName", E.string userInfo.lastName )
        , ( "role", E.string userInfo.role )
        , ( "permissions", E.list E.string userInfo.permissions )
        ]
