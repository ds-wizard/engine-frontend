module Shared.Data.Event.AddReferenceResourcePageEventData exposing
    ( AddReferenceResourcePageEventData
    , decoder
    , encode
    , toReference
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Reference exposing (Reference(..))


type alias AddReferenceResourcePageEventData =
    { shortUuid : String
    , annotations : List Annotation
    }


decoder : Decoder AddReferenceResourcePageEventData
decoder =
    D.succeed AddReferenceResourcePageEventData
        |> D.required "shortUuid" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "shortUuid", E.string data.shortUuid )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toReference : String -> AddReferenceResourcePageEventData -> Reference
toReference uuid data =
    ResourcePageReference
        { uuid = uuid
        , shortUuid = data.shortUuid
        , annotations = data.annotations
        }
