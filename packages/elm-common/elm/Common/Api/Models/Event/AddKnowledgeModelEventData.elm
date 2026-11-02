module Common.Api.Models.Event.AddKnowledgeModelEventData exposing
    ( AddKnowledgeModelEventData
    , decoder
    , encode
    )

import Common.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddKnowledgeModelEventData =
    { annotations : List Annotation }


decoder : Decoder AddKnowledgeModelEventData
decoder =
    D.succeed AddKnowledgeModelEventData
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddKnowledgeModelEvent" )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]
