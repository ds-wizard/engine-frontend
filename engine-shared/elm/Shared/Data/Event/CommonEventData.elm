module Shared.Data.Event.CommonEventData exposing
    ( CommonEventData
    , decoder
    , encode
    )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time


type alias CommonEventData =
    { uuid : String
    , parentUuid : String
    , entityUuid : String
    , createdAt : Time.Posix
    }


decoder : Decoder CommonEventData
decoder =
    D.succeed CommonEventData
        |> D.required "uuid" D.string
        |> D.required "parentUuid" D.string
        |> D.required "entityUuid" D.string
        |> D.required "createdAt" D.datetime


encode : CommonEventData -> List ( String, E.Value )
encode data =
    [ ( "uuid", E.string data.uuid )
    , ( "parentUuid", E.string data.parentUuid )
    , ( "entityUuid", E.string data.entityUuid )
    , ( "createdAt", Iso8601.encode data.createdAt )
    ]
