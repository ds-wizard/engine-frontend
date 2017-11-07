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


type alias PackageDetail =
    { name : String
    , shortName : String
    , versions : List PackageVersion
    }


type alias PackageVersion =
    { version : String
    , description : String
    }


packageDetailDecoder : Decoder PackageDetail
packageDetailDecoder =
    decode PackageDetail
        |> required "name" Decode.string
        |> required "shortName" Decode.string
        |> required "versions" (Decode.list packageVersionDecoder)


packageVersionDecoder : Decoder PackageVersion
packageVersionDecoder =
    decode PackageVersion
        |> required "version" Decode.string
        |> required "description" Decode.string
