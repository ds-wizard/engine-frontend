module Shared.Data.Permission exposing (Permission, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Member as Member exposing (Member)
import Uuid exposing (Uuid)


type alias Permission =
    { uuid : Uuid
    , questionnaireUuid : Uuid
    , member : Member
    , perms : List String
    }


decoder : Decoder Permission
decoder =
    D.succeed Permission
        |> D.required "uuid" Uuid.decoder
        |> D.required "questionnaireUuid" Uuid.decoder
        |> D.required "member" Member.decoder
        |> D.required "perms" (D.list D.string)


encode : Permission -> E.Value
encode permission =
    E.object
        [ ( "uuid", Uuid.encode permission.uuid )
        , ( "questionnaireUuid", Uuid.encode permission.questionnaireUuid )
        , ( "member", Member.encode permission.member )
        , ( "perms", E.list E.string permission.perms )
        ]
