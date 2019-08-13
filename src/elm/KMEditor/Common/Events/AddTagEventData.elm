module KMEditor.Common.Events.AddTagEventData exposing
    ( AddTagEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias AddTagEventData =
    { name : String
    , description : Maybe String
    , color : String
    }


decoder : Decoder AddTagEventData
decoder =
    D.succeed AddTagEventData
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "color" D.string


encode : AddTagEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "AddTagEvent" )
    , ( "name", E.string data.name )
    , ( "description", E.maybe E.string data.description )
    , ( "color", E.string data.color )
    ]
