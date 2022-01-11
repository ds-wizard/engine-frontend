module Shared.Data.Event.EditReferenceResourcePageEventData exposing
    ( EditReferenceResourcePageEventData
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditReferenceResourcePageEventData =
    { shortUuid : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditReferenceResourcePageEventData
decoder =
    D.succeed EditReferenceResourcePageEventData
        |> D.required "shortUuid" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "shortUuid", EventField.encode E.string data.shortUuid )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditReferenceResourcePageEventData
init =
    { shortUuid = EventField.empty
    , annotations = EventField.empty
    }
