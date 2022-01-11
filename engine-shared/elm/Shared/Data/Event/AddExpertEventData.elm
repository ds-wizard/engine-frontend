module Shared.Data.Event.AddExpertEventData exposing
    ( AddExpertEventData
    , decoder
    , encode
    , init
    , toExpert
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)


type alias AddExpertEventData =
    { name : String
    , email : String
    , annotations : List Annotation
    }


decoder : Decoder AddExpertEventData
decoder =
    D.succeed AddExpertEventData
        |> D.required "name" D.string
        |> D.required "email" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddExpertEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddExpertEvent" )
    , ( "name", E.string data.name )
    , ( "email", E.string data.email )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddExpertEventData
init =
    { name = ""
    , email = ""
    , annotations = []
    }


toExpert : String -> AddExpertEventData -> Expert
toExpert uuid data =
    { uuid = uuid
    , name = data.name
    , email = data.email
    , annotations = data.annotations
    }
