module Shared.Data.Event.AddReferenceCrossEventData exposing
    ( AddReferenceCrossEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddReferenceCrossEventData =
    { targetUuid : String
    , description : String
    , annotations : Dict String String
    }


decoder : Decoder AddReferenceCrossEventData
decoder =
    D.succeed AddReferenceCrossEventData
        |> D.required "targetUuid" D.string
        |> D.required "description" D.string
        |> D.required "annotations" (D.dict D.string)


encode : AddReferenceCrossEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "CrossReference" )
    , ( "targetUuid", E.string data.targetUuid )
    , ( "description", E.string data.description )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
