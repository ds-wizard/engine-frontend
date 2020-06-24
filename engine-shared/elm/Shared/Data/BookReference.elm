module Shared.Data.BookReference exposing (..)

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D


type alias BookReference =
    { shortUuid : String
    , content : String
    , bookChapter : String
    }


decoder : Decoder BookReference
decoder =
    D.succeed BookReference
        |> D.required "shortUuid" D.string
        |> D.required "content" D.string
        |> D.required "bookChapter" D.string
