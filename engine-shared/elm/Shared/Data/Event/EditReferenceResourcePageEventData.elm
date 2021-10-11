module Shared.Data.Event.EditReferenceResourcePageEventData exposing
    ( EditReferenceResourcePageEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditReferenceResourcePageEventData =
    { shortUuid : EventField String
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditReferenceResourcePageEventData
decoder =
    D.succeed EditReferenceResourcePageEventData
        |> D.required "shortUuid" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "shortUuid", EventField.encode E.string data.shortUuid )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
