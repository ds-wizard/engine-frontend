module Shared.Data.Event.AddReferenceResourcePageEventData exposing
    ( AddReferenceResourcePageEventData
    , decoder
    , encode
    , toReference
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Reference exposing (Reference(..))


type alias AddReferenceResourcePageEventData =
    { resourcePageUuid : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder AddReferenceResourcePageEventData
decoder =
    D.succeed AddReferenceResourcePageEventData
        |> D.required "resourcePageUuid" (D.nullable D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "resourcePageUuid", E.maybe E.string data.resourcePageUuid )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toReference : String -> AddReferenceResourcePageEventData -> Reference
toReference uuid data =
    ResourcePageReference
        { uuid = uuid
        , resourcePageUuid = data.resourcePageUuid
        , annotations = data.annotations
        }
