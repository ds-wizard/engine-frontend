module Shared.Data.Event.EditReferenceURLEventData exposing
    ( EditReferenceURLEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditReferenceURLEventData =
    { url : EventField String
    , label : EventField String
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditReferenceURLEventData
decoder =
    D.succeed EditReferenceURLEventData
        |> D.required "url" (EventField.decoder D.string)
        |> D.required "label" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditReferenceURLEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "URLReference" )
    , ( "url", EventField.encode E.string data.url )
    , ( "label", EventField.encode E.string data.label )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
