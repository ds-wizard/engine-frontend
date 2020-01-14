module Wizard.KMEditor.Common.Events.AddReferenceCrossEventData exposing
    ( AddReferenceCrossEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddReferenceCrossEventData =
    { targetUuid : String
    , description : String
    }


decoder : Decoder AddReferenceCrossEventData
decoder =
    D.succeed AddReferenceCrossEventData
        |> D.required "targetUuid" D.string
        |> D.required "description" D.string


encode : AddReferenceCrossEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "CrossReference" )
    , ( "targetUuid", E.string data.targetUuid )
    , ( "description", E.string data.description )
    ]
