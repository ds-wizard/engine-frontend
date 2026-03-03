module Wizard.Api.Models.DocumentTemplateDraft exposing
    ( DocumentTemplateDraft
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateDraft =
    { createdAt : Time.Posix
    , description : String
    , uuid : Uuid
    , name : String
    , organizationId : String
    , templateId : String
    , version : Version
    , updatedAt : Time.Posix
    }


decoder : Decoder DocumentTemplateDraft
decoder =
    D.succeed DocumentTemplateDraft
        |> D.required "createdAt" D.datetime
        |> D.required "description" D.string
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder
        |> D.required "updatedAt" D.datetime
