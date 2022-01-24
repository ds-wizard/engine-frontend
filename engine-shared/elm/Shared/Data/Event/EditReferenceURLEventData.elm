module Shared.Data.Event.EditReferenceURLEventData exposing
    ( EditReferenceURLEventData
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditReferenceURLEventData =
    { url : EventField String
    , label : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditReferenceURLEventData
decoder =
    D.succeed EditReferenceURLEventData
        |> D.required "url" (EventField.decoder D.string)
        |> D.required "label" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditReferenceURLEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "URLReference" )
    , ( "url", EventField.encode E.string data.url )
    , ( "label", EventField.encode E.string data.label )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditReferenceURLEventData
init =
    { url = EventField.empty
    , label = EventField.empty
    , annotations = EventField.empty
    }
