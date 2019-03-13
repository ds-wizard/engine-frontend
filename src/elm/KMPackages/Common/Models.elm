module KMPackages.Common.Models exposing
    ( Package
    , PackageDetail
    , packageDecoder
    , packageDetailDecoder
    , packageDetailListDecoder
    , packageListDecoder
    )

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)


type alias Package =
    { name : String
    , organizationId : String
    , kmId : String
    , latestVersion : String
    }


packageDecoder : Decoder Package
packageDecoder =
    Decode.succeed Package
        |> required "name" Decode.string
        |> required "organizationId" Decode.string
        |> required "kmId" Decode.string
        |> required "latestVersion" Decode.string


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
    , metamodelVersion : Int
    }


packageDetailDecoder : Decoder PackageDetail
packageDetailDecoder =
    Decode.succeed PackageDetail
        |> required "name" Decode.string
        |> required "id" Decode.string
        |> required "organizationId" Decode.string
        |> required "kmId" Decode.string
        |> required "version" Decode.string
        |> required "description" Decode.string
        |> required "metamodelVersion" Decode.int


packageDetailListDecoder : Decoder (List PackageDetail)
packageDetailListDecoder =
    Decode.list packageDetailDecoder
