module PackageManagement.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Package =
    { name : String
    , organizationId : String
    , kmId : String
    }


packageDecoder : Decoder Package
packageDecoder =
    decode Package
        |> required "name" Decode.string
        |> required "organizationId" Decode.string
        |> required "kmId" Decode.string


packageListDecoder : Decoder (List Package)
packageListDecoder =
    Decode.list packageDecoder


type alias PackageDetail =
    { name : String
    , id : String
    , organizationId : String
    , kmId : String
    , version : String
    , description : String
    }


packageDetailDecoder : Decoder PackageDetail
packageDetailDecoder =
    decode PackageDetail
        |> required "name" Decode.string
        |> required "id" Decode.string
        |> required "organizationId" Decode.string
        |> required "kmId" Decode.string
        |> required "version" Decode.string
        |> required "description" Decode.string


packageDetailListDecoder : Decoder (List PackageDetail)
packageDetailListDecoder =
    Decode.list packageDetailDecoder
