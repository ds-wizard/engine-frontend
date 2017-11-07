module PackageManagement.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Package =
    { name : String
    , shortName : String
    }


packageDecoder : Decoder Package
packageDecoder =
    decode Package
        |> required "name" Decode.string
        |> required "shortName" Decode.string


packageListDecoder : Decoder (List Package)
packageListDecoder =
    Decode.list packageDecoder
