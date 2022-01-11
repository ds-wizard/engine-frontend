module Shared.Data.Event.AddReferenceURLEventData exposing
    ( AddReferenceURLEventData
    , decoder
    , encode
    , init
    , toReference
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Reference exposing (Reference(..))


type alias AddReferenceURLEventData =
    { url : String
    , label : String
    , annotations : List Annotation
    }


decoder : Decoder AddReferenceURLEventData
decoder =
    D.succeed AddReferenceURLEventData
        |> D.required "url" D.string
        |> D.required "label" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddReferenceURLEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "URLReference" )
    , ( "url", E.string data.url )
    , ( "label", E.string data.label )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddReferenceURLEventData
init =
    { url = ""
    , label = ""
    , annotations = []
    }


toReference : String -> AddReferenceURLEventData -> Reference
toReference uuid data =
    URLReference
        { uuid = uuid
        , url = data.url
        , label = data.label
        , annotations = data.annotations
        }
