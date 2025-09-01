module Wizard.Data.Navigator exposing
    ( Navigator
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Navigator =
    { pdf : Bool }


default : Navigator
default =
    { pdf = True }


decoder : Decoder Navigator
decoder =
    D.succeed Navigator
        |> D.required "pdf" D.bool
