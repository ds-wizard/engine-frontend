module Wizard.Api.Models.DocumentTemplate.DocumentTemplatePackage exposing
    ( DocumentTemplatePackage
    , compareById
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplatePackage =
    { uuid : Uuid
    , name : String
    , description : String
    , organizationId : String
    , kmId : String
    , version : Version
    }


decoder : Decoder DocumentTemplatePackage
decoder =
    D.succeed DocumentTemplatePackage
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "organizationId" D.string
        |> D.required "kmId" D.string
        |> D.required "version" Version.decoder


compareById : DocumentTemplatePackage -> DocumentTemplatePackage -> Order
compareById tp1 tp2 =
    if tp1.organizationId /= tp2.organizationId then
        Basics.compare tp1.organizationId tp2.organizationId

    else if tp1.kmId /= tp2.kmId then
        Basics.compare tp1.kmId tp2.kmId

    else
        Version.compare tp1.version tp2.version
