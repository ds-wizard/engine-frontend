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
    , version : String
    , description : String
    }


packageDetailDecoder : Decoder PackageDetail
packageDetailDecoder =
    decode PackageDetail
        |> required "name" Decode.string
        |> required "shortName" Decode.string
        |> required "version" Decode.string
        |> required "description" Decode.string


packageDetailListDecoder : Decoder (List PackageDetail)
packageDetailListDecoder =
    Decode.list packageDetailDecoder


getPackageName : List PackageDetail -> ( String, String )
getPackageName packages =
    case packages of
        first :: _ ->
            ( first.name, first.shortName )

        _ ->
            ( "", "" )


getPackageShortName : List PackageDetail -> String
getPackageShortName =
    getPackageName >> Tuple.second
