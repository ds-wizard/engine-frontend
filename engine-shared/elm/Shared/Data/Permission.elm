module Shared.Data.Permission exposing (Permission, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Member as Member exposing (Member)


type alias Permission =
    { member : Member
    , perms : List String
    }


decoder : Decoder Permission
decoder =
    D.succeed Permission
        |> D.required "member" Member.decoder
        |> D.required "perms" (D.list D.string)


encode : Permission -> E.Value
encode permission =
    E.object
        [ ( "member", Member.encode permission.member )
        , ( "perms", E.list E.string permission.perms )
        ]
