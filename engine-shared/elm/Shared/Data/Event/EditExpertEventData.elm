module Shared.Data.Event.EditExpertEventData exposing
    ( EditExpertEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditExpertEventData =
    { name : EventField String
    , email : EventField String
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditExpertEventData
decoder =
    D.succeed EditExpertEventData
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "email" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditExpertEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditExpertEvent" )
    , ( "name", EventField.encode E.string data.name )
    , ( "email", EventField.encode E.string data.email )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
