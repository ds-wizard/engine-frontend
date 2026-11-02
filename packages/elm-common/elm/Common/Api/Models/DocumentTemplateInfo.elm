module Common.Api.Models.DocumentTemplateInfo exposing (DocumentTemplateInfo, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias DocumentTemplateInfo =
    { uuid : Uuid
    , name : String
    , id : String
    , version : Version
    }


decoder : Decoder DocumentTemplateInfo
decoder =
    D.succeed DocumentTemplateInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "id" D.string
        |> D.required "version" Version.decoder


encode : DocumentTemplateInfo -> E.Value
encode templateInfo =
    E.object
        [ ( "uuid", Uuid.encode templateInfo.uuid )
        , ( "name", E.string templateInfo.name )
        , ( "id", E.string templateInfo.id )
        , ( "version", Version.encode templateInfo.version )
        ]
