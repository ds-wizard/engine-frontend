module Shared.Data.Event.AddKnowledgeModelEventData exposing
    ( AddKnowledgeModelEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddKnowledgeModelEventData =
    { annotations : Dict String String }


decoder : Decoder AddKnowledgeModelEventData
decoder =
    D.succeed AddKnowledgeModelEventData
        |> D.required "annotations" (D.dict D.string)


encode : AddKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddKnowledgeModelEvent" )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
