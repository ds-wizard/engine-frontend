module Shared.Data.Event.AddReferenceCrossEventData exposing
    ( AddReferenceCrossEventData
    , decoder
    , encode
    , toReference
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Reference exposing (Reference(..))


type alias AddReferenceCrossEventData =
    { targetUuid : String
    , description : String
    , annotations : List Annotation
    }


decoder : Decoder AddReferenceCrossEventData
decoder =
    D.succeed AddReferenceCrossEventData
        |> D.required "targetUuid" D.string
        |> D.required "description" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddReferenceCrossEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "CrossReference" )
    , ( "targetUuid", E.string data.targetUuid )
    , ( "description", E.string data.description )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toReference : String -> AddReferenceCrossEventData -> Reference
toReference uuid data =
    CrossReference
        { uuid = uuid
        , targetUuid = data.targetUuid
        , description = data.description
        , annotations = data.annotations
        }
