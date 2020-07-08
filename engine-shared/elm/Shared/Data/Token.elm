module Shared.Data.Token exposing
    ( Token
    , decoder
    , empty
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias Token =
    { token : String
    }


empty : Token
empty =
    { token = ""
    }


decoder : Decoder Token
decoder =
    D.succeed Token
        |> D.required "token" D.string


encode : Token -> E.Value
encode token =
    E.object
        [ ( "token", E.string token.token )
        ]
