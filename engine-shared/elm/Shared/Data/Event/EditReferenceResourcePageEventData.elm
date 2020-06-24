module Shared.Data.Event.EditReferenceResourcePageEventData exposing
    ( EditReferenceResourcePageEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditReferenceResourcePageEventData =
    { shortUuid : EventField String
    }


decoder : Decoder EditReferenceResourcePageEventData
decoder =
    D.succeed EditReferenceResourcePageEventData
        |> D.required "shortUuid" (EventField.decoder D.string)


encode : EditReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "shortUuid", EventField.encode E.string data.shortUuid )
    ]
