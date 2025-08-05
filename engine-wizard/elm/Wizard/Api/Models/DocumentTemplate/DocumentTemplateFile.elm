module Wizard.Api.Models.DocumentTemplate.DocumentTemplateFile exposing
    ( DocumentTemplateFile
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias DocumentTemplateFile =
    { uuid : Uuid
    , fileName : String
    }


decoder : Decoder DocumentTemplateFile
decoder =
    D.succeed DocumentTemplateFile
        |> D.required "uuid" Uuid.decoder
        |> D.required "fileName" D.string


encode : DocumentTemplateFile -> String -> E.Value
encode file content =
    E.object
        [ ( "fileName", E.string file.fileName )
        , ( "content", E.string content )
        ]
