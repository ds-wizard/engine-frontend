module Shared.Data.Event.AddChoiceEventData exposing
    ( AddChoiceEventData
    , decoder
    , encode
    , init
    , toChoice
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)


type alias AddChoiceEventData =
    { label : String
    , annotations : List Annotation
    }


decoder : Decoder AddChoiceEventData
decoder =
    D.succeed AddChoiceEventData
        |> D.required "label" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddChoiceEvent" )
    , ( "label", E.string data.label )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddChoiceEventData
init =
    { label = ""
    , annotations = []
    }


toChoice : String -> AddChoiceEventData -> Choice
toChoice uuid data =
    { uuid = uuid
    , label = data.label
    , annotations = data.annotations
    }
