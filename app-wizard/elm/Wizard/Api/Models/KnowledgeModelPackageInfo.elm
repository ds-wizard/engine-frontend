module Wizard.Api.Models.KnowledgeModelPackageInfo exposing
    ( KnowledgeModelPackageInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Version exposing (Version)


type alias KnowledgeModelPackageInfo =
    { id : String
    , name : String
    , version : Version
    }


decoder : Decoder KnowledgeModelPackageInfo
decoder =
    D.succeed KnowledgeModelPackageInfo
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
