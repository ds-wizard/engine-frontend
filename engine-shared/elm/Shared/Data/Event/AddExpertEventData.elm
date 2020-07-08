module Shared.Data.Event.AddExpertEventData exposing
    ( AddExpertEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddExpertEventData =
    { name : String
    , email : String
    }


decoder : Decoder AddExpertEventData
decoder =
    D.succeed AddExpertEventData
        |> D.required "name" D.string
        |> D.required "email" D.string


encode : AddExpertEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddExpertEvent" )
    , ( "name", E.string data.name )
    , ( "email", E.string data.email )
    ]
