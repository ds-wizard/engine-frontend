module Wizard.Api.Models.Event.EditReferenceCrossEventData exposing
    ( EditReferenceCrossEventData
    , decoder
    , encode
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditReferenceCrossEventData =
    { targetUuid : EventField String
    , description : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditReferenceCrossEventData
decoder =
    D.succeed EditReferenceCrossEventData
        |> D.required "targetUuid" (EventField.decoder D.string)
        |> D.required "description" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditReferenceCrossEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "CrossReference" )
    , ( "targetUuid", EventField.encode E.string data.targetUuid )
    , ( "description", EventField.encode E.string data.description )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


squash : EditReferenceCrossEventData -> EditReferenceCrossEventData -> EditReferenceCrossEventData
squash oldData newData =
    { targetUuid = EventField.squash oldData.targetUuid newData.targetUuid
    , description = EventField.squash oldData.description newData.description
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
