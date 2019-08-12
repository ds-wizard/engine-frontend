module KMEditor.Common.Events.AddReferenceResourcePageEventData exposing
    ( AddReferenceResourcePageEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddReferenceResourcePageEventData =
    { shortUuid : String
    }


decoder : Decoder AddReferenceResourcePageEventData
decoder =
    D.succeed AddReferenceResourcePageEventData
        |> D.required "shortUuid" D.string


encode : AddReferenceResourcePageEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "ResourcePageReference" )
    , ( "shortUuid", E.string data.shortUuid )
    ]
