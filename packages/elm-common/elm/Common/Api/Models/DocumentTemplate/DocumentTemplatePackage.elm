module Common.Api.Models.DocumentTemplate.DocumentTemplatePackage exposing
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
    , id : String
    , version : Version
    }


decoder : Decoder DocumentTemplatePackage
decoder =
    D.succeed DocumentTemplatePackage
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "id" D.string
        |> D.required "version" Version.decoder


compareById : DocumentTemplatePackage -> DocumentTemplatePackage -> Order
compareById tp1 tp2 =
    if tp1.id /= tp2.id then
        Basics.compare tp1.id tp2.id

    else
        Version.compare tp1.version tp2.version
