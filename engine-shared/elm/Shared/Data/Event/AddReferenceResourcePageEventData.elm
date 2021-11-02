module Shared.Data.Event.AddReferenceResourcePageEventData exposing
    ( AddReferenceResourcePageEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddReferenceResourcePageEventData =
    { shortUuid : String
    , annotations : Dict String String
    }


decoder : Decoder AddReferenceResourcePageEventData
decoder =
    D.succeed AddReferenceResourcePageEventData
        |> D.required "shortUuid" D.string
        |> D.required "annotations" (D.dict D.string)


encode : AddReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "shortUuid", E.string data.shortUuid )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
