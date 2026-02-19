module Wizard.Api.Models.DocumentTemplateInfo exposing (DocumentTemplateInfo, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Version exposing (Version)


type alias DocumentTemplateInfo =
    { id : String
    , name : String
    , organizationId : String
    , templateId : String
    , version : Version
    }


decoder : Decoder DocumentTemplateInfo
decoder =
    D.succeed DocumentTemplateInfo
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "organizationId" D.string
        |> D.required "templateId" D.string
        |> D.required "version" Version.decoder


encode : DocumentTemplateInfo -> E.Value
encode templateInfo =
    E.object
        [ ( "id", E.string templateInfo.id )
        , ( "name", E.string templateInfo.name )
        , ( "organizationId", E.string templateInfo.organizationId )
        , ( "templateId", E.string templateInfo.templateId )
        , ( "version", Version.encode templateInfo.version )
        ]
