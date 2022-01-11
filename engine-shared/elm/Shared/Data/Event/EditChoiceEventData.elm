module Shared.Data.Event.EditChoiceEventData exposing
    ( EditChoiceEventData
    , apply
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)


type alias EditChoiceEventData =
    { label : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditChoiceEventData
decoder =
    D.succeed EditChoiceEventData
        |> D.required "label" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditChoiceEvent" )
    , ( "label", EventField.encode E.string data.label )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditChoiceEventData
init =
    { label = EventField.empty
    , annotations = EventField.empty
    }


apply : EditChoiceEventData -> Choice -> Choice
apply eventData choice =
    { choice
        | label = EventField.getValueWithDefault eventData.label choice.label
        , annotations = EventField.getValueWithDefault eventData.annotations choice.annotations
    }
