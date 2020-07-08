module Shared.Data.Event.MoveEventData exposing
    ( MoveEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias MoveEventData =
    { targetUuid : String
    }


decoder : Decoder MoveEventData
decoder =
    D.succeed MoveEventData
        |> D.required "targetUuid" D.string


encode : String -> MoveEventData -> List ( String, E.Value )
encode eventType data =
    [ ( "eventType", E.string eventType )
    , ( "targetUuid", E.string data.targetUuid )
    ]
