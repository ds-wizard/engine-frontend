module Shared.Data.Token exposing
    ( Token
    , create
    , decoder
    , empty
    , encode
    )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time


type alias Token =
    { token : String
    , expiresAt : Time.Posix
    }


empty : Token
empty =
    { token = ""
    , expiresAt = Time.millisToPosix 0
    }


create : String -> Time.Posix -> Token
create token expiresAt =
    { token = token
    , expiresAt = expiresAt
    }


decoder : Decoder Token
decoder =
    D.succeed Token
        |> D.required "token" D.string
        |> D.required "expiresAt" D.datetime


encode : Token -> E.Value
encode token =
    E.object
        [ ( "token", E.string token.token )
        , ( "expiresAt", Iso8601.encode token.expiresAt )
        ]
