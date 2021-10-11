module Shared.Data.Event.AddPhaseEventData exposing
    ( AddPhaseEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddPhaseEventData =
    { title : String
    , description : Maybe String
    , annotations : Dict String String
    }


decoder : Decoder AddPhaseEventData
decoder =
    D.succeed AddPhaseEventData
        |> D.required "title" D.string
        |> D.required "description" (D.maybe D.string)
        |> D.required "annotations" (D.dict D.string)


encode : AddPhaseEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddPhaseEvent" )
    , ( "title", E.string data.title )
    , ( "description", E.maybe E.string data.description )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
