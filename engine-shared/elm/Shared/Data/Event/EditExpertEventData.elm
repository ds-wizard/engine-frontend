module Shared.Data.Event.EditExpertEventData exposing
    ( EditExpertEventData
    , apply
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)


type alias EditExpertEventData =
    { name : EventField String
    , email : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditExpertEventData
decoder =
    D.succeed EditExpertEventData
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "email" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditExpertEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditExpertEvent" )
    , ( "name", EventField.encode E.string data.name )
    , ( "email", EventField.encode E.string data.email )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditExpertEventData
init =
    { name = EventField.empty
    , email = EventField.empty
    , annotations = EventField.empty
    }


apply : EditExpertEventData -> Expert -> Expert
apply eventData expert =
    { expert
        | name = EventField.getValueWithDefault eventData.name expert.name
        , email = EventField.getValueWithDefault eventData.email expert.email
        , annotations = EventField.getValueWithDefault eventData.annotations expert.annotations
    }


squash : EditExpertEventData -> EditExpertEventData -> EditExpertEventData
squash oldData newData =
    { name = EventField.squash oldData.name newData.name
    , email = EventField.squash oldData.email newData.email
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
