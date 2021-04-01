module Shared.Data.Event.AddKnowledgeModelEventData exposing
    ( AddKnowledgeModelEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type alias AddKnowledgeModelEventData =
    {}


decoder : Decoder AddKnowledgeModelEventData
decoder =
    D.succeed AddKnowledgeModelEventData


encode : AddKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddKnowledgeModelEvent" )
    ]
