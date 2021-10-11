module Shared.Data.Event.AddChoiceEventData exposing
    ( AddChoiceEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddChoiceEventData =
    { label : String
    , annotations : Dict String String
    }


decoder : Decoder AddChoiceEventData
decoder =
    D.succeed AddChoiceEventData
        |> D.required "label" D.string
        |> D.required "annotations" (D.dict D.string)


encode : AddChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddChoiceEvent" )
    , ( "label", E.string data.label )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
