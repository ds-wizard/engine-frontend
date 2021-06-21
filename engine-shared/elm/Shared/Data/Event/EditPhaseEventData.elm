module Shared.Data.Event.EditPhaseEventData exposing
    ( EditPhaseEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditPhaseEventData =
    { title : EventField String
    , description : EventField (Maybe String)
    }


decoder : Decoder EditPhaseEventData
decoder =
    D.succeed EditPhaseEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "description" (EventField.decoder (D.maybe D.string))


encode : EditPhaseEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditPhaseEvent" )
    , ( "title", EventField.encode E.string data.title )
    , ( "description", EventField.encode (E.maybe E.string) data.description )
    ]
