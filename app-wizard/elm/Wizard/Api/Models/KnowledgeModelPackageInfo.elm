module Wizard.Api.Models.KnowledgeModelPackageInfo exposing
    ( KnowledgeModelPackageInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias KnowledgeModelPackageInfo =
    { uuid : Uuid
    , name : String
    , version : Version
    }


decoder : Decoder KnowledgeModelPackageInfo
decoder =
    D.succeed KnowledgeModelPackageInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
