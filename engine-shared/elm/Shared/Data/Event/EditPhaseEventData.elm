module Shared.Data.Event.EditPhaseEventData exposing
    ( EditPhaseEventData
    , apply
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)


type alias EditPhaseEventData =
    { title : EventField String
    , description : EventField (Maybe String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditPhaseEventData
decoder =
    D.succeed EditPhaseEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "description" (EventField.decoder (D.maybe D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditPhaseEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditPhaseEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "description", EventField.encode (E.maybe E.string) data.description )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditPhaseEventData
init =
    { title = EventField.empty
    , description = EventField.empty
    , annotations = EventField.empty
    }


apply : EditPhaseEventData -> Phase -> Phase
apply eventData phase =
    { phase
        | title = EventField.getValueWithDefault eventData.title phase.title
        , description = EventField.getValueWithDefault eventData.description phase.description
        , annotations = EventField.getValueWithDefault eventData.annotations phase.annotations
    }
