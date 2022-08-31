module Shared.Data.PackageSuggestion exposing
    ( PackageSuggestion
    , decoder
    , fromPackage
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Package exposing (Package)
import Version exposing (Version)


type alias PackageSuggestion =
    { id : String
    , name : String
    , description : String
    , version : Version
    , versions : List Version
    }


decoder : Decoder PackageSuggestion
decoder =
    D.succeed PackageSuggestion
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "version" Version.decoder
        |> D.required "versions" (D.list Version.decoder)


fromPackage : Package -> List Version -> PackageSuggestion
fromPackage package packageVersions =
    { id = package.id
    , name = package.name
    , description = package.description
    , version = package.version
    , versions = packageVersions
    }
