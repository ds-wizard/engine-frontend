module Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair exposing
    ( KeyValuePair
    , decoder
    , encode
    , fromTuple
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias KeyValuePair =
    { key : String
    , value : String
    }


decoder : Decoder KeyValuePair
decoder =
    D.succeed KeyValuePair
        |> D.required "key" D.string
        |> D.required "value" D.string


encode : KeyValuePair -> E.Value
encode annotation =
    E.object
        [ ( "key", E.string annotation.key )
        , ( "value", E.string annotation.value )
        ]


new : KeyValuePair
new =
    { key = ""
    , value = ""
    }


fromTuple : ( String, String ) -> KeyValuePair
fromTuple ( key, value ) =
    { key = key
    , value = value
    }
