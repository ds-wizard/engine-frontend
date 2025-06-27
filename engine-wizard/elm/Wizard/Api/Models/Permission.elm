module Wizard.Api.Models.Permission exposing (Permission, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.Member as Member exposing (Member)


type alias Permission =
    { member : Member
    , perms : List String
    }


decoder : Decoder Permission
decoder =
    D.succeed Permission
        |> D.required "member" Member.decoder
        |> D.required "perms" (D.list D.string)
