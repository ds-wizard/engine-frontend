module Shared.Data.KnowledgeModel.Integration.RequestHeader exposing
    ( RequestHeader
    , decoder
    , encode
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias RequestHeader =
    { key : String
    , value : String
    }


decoder : Decoder RequestHeader
decoder =
    D.succeed RequestHeader
        |> D.required "key" D.string
        |> D.required "value" D.string


encode : RequestHeader -> E.Value
encode annotation =
    E.object
        [ ( "key", E.string annotation.key )
        , ( "value", E.string annotation.value )
        ]


new : RequestHeader
new =
    { key = ""
    , value = ""
    }
