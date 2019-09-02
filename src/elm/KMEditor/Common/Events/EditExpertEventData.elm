module KMEditor.Common.Events.EditExpertEventData exposing
    ( EditExpertEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import KMEditor.Common.Events.EventField as EventField exposing (EventField)


type alias EditExpertEventData =
    { name : EventField String
    , email : EventField String
    }


decoder : Decoder EditExpertEventData
decoder =
    D.succeed EditExpertEventData
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "email" (EventField.decoder D.string)


encode : EditExpertEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditExpertEvent" )
    , ( "name", EventField.encode E.string data.name )
    , ( "email", EventField.encode E.string data.email )
    ]
