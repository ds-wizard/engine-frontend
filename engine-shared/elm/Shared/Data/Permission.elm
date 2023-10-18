module Shared.Data.Permission exposing (Permission, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Member as Member exposing (Member)
import Uuid exposing (Uuid)


type alias Permission =
    { questionnaireUuid : Uuid
    , member : Member
    , perms : List String
    }


decoder : Decoder Permission
decoder =
    D.succeed Permission
        |> D.required "questionnaireUuid" Uuid.decoder
        |> D.required "member" Member.decoder
        |> D.required "perms" (D.list D.string)
