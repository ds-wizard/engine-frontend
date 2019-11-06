module Wizard.KMEditor.Common.Events.EditReferenceURLEventData exposing
    ( EditReferenceURLEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.KMEditor.Common.Events.EventField as EventField exposing (EventField)


type alias EditReferenceURLEventData =
    { url : EventField String
    , label : EventField String
    }


decoder : Decoder EditReferenceURLEventData
decoder =
    D.succeed EditReferenceURLEventData
        |> D.required "url" (EventField.decoder D.string)
        |> D.required "label" (EventField.decoder D.string)


encode : EditReferenceURLEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "URLReference" )
    , ( "url", EventField.encode E.string data.url )
    , ( "label", EventField.encode E.string data.label )
    ]
