module Shared.Data.Event.EditReferenceCrossEventData exposing
    ( EditReferenceCrossEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditReferenceCrossEventData =
    { targetUuid : EventField String
    , description : EventField String
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditReferenceCrossEventData
decoder =
    D.succeed EditReferenceCrossEventData
        |> D.required "targetUuid" (EventField.decoder D.string)
        |> D.required "description" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditReferenceCrossEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "CrossReference" )
    , ( "targetUuid", EventField.encode E.string data.targetUuid )
    , ( "description", EventField.encode E.string data.description )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
