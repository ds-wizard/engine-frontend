module Shared.Data.Event.EditMetricEventData exposing
    ( EditMetricEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditMetricEventData =
    { title : EventField String
    , abbreviation : EventField (Maybe String)
    , description : EventField (Maybe String)
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditMetricEventData
decoder =
    D.succeed EditMetricEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "abbreviation" (EventField.decoder (D.maybe D.string))
        |> D.required "description" (EventField.decoder (D.maybe D.string))
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditMetricEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditMetricEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "abbreviation", EventField.encode (E.maybe E.string) data.abbreviation )
    , ( "description", EventField.encode (E.maybe E.string) data.description )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
