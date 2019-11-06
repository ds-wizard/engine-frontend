module Wizard.KMEditor.Common.Events.AddReferenceURLEventData exposing
    ( AddReferenceURLEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddReferenceURLEventData =
    { url : String
    , label : String
    }


decoder : Decoder AddReferenceURLEventData
decoder =
    D.succeed AddReferenceURLEventData
        |> D.required "url" D.string
        |> D.required "label" D.string


encode : AddReferenceURLEventData -> List ( String, E.Value )
encode data =
    [ ( "referenceType", E.string "URLReference" )
    , ( "url", E.string data.url )
    , ( "label", E.string data.label )
    ]
