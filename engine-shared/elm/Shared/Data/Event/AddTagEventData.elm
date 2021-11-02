module Shared.Data.Event.AddTagEventData exposing
    ( AddTagEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddTagEventData =
    { name : String
    , description : Maybe String
    , color : String
    , annotations : Dict String String
    }


decoder : Decoder AddTagEventData
decoder =
    D.succeed AddTagEventData
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "color" D.string
        |> D.required "annotations" (D.dict D.string)


encode : AddTagEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddTagEvent" )
    , ( "name", E.string data.name )
    , ( "description", E.maybe E.string data.description )
    , ( "color", E.string data.color )
    , ( "annotations", E.dict identity E.string data.annotations )
    ]
