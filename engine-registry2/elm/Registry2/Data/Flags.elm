module Registry2.Data.Flags exposing
    ( Flags
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Flags =
    { apiUrl : String }


decoder : Decoder Flags
decoder =
    D.succeed Flags
        |> D.required "apiUrl" D.string


default : Flags
default =
    { apiUrl = "" }
