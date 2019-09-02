module KMEditor.Common.Events.AddKnowledgeModelEventData exposing
    ( AddKnowledgeModelEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddKnowledgeModelEventData =
    { name : String
    }


decoder : Decoder AddKnowledgeModelEventData
decoder =
    D.succeed AddKnowledgeModelEventData
        |> D.required "name" D.string


encode : AddKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddKnowledgeModelEvent" )
    , ( "name", E.string data.name )
    ]
