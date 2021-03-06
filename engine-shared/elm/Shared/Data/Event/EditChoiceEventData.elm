module Shared.Data.Event.EditChoiceEventData exposing
    ( EditChoiceEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditChoiceEventData =
    { label : EventField String
    }


decoder : Decoder EditChoiceEventData
decoder =
    D.succeed EditChoiceEventData
        |> D.required "label" (EventField.decoder D.string)


encode : EditChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditChoiceEvent" )
    , ( "label", EventField.encode E.string data.label )
    ]
