module PackageManagement.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Package =
    { name : String
    , groupId : String
    , artifactId : String
    }


packageDecoder : Decoder Package
packageDecoder =
    decode Package
        |> required "name" Decode.string
        |> required "groupId" Decode.string
        |> required "artifactId" Decode.string


packageListDecoder : Decoder (List Package)
packageListDecoder =
    Decode.list packageDecoder


type alias PackageDetail =
    { name : String
    , packageId : String
    , groupId : String
    , artifactId : String
    , version : String
    , description : String
    }


packageDetailDecoder : Decoder PackageDetail
packageDetailDecoder =
    decode PackageDetail
        |> required "name" Decode.string
        |> required "packageId" Decode.string
        |> required "groupId" Decode.string
        |> required "artifactId" Decode.string
        |> required "version" Decode.string
        |> required "description" Decode.string


packageDetailListDecoder : Decoder (List PackageDetail)
packageDetailListDecoder =
    Decode.list packageDetailDecoder
