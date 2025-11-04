module Common.Data.Navigator exposing
    ( Navigator
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Navigator =
    { pdf : Bool
    , isMac : Bool
    }


default : Navigator
default =
    { pdf = True
    , isMac = False
    }


decoder : Decoder Navigator
decoder =
    D.succeed Navigator
        |> D.required "pdf" D.bool
        |> D.required "isMac" D.bool
