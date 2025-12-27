module Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormat exposing
    ( DocumentTemplateFormat
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)


type alias DocumentTemplateFormat =
    { uuid : Uuid
    , name : String
    , icon : String
    }


decoder : Decoder DocumentTemplateFormat
decoder =
    D.succeed DocumentTemplateFormat
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "icon" D.string


encode : DocumentTemplateFormat -> E.Value
encode format =
    E.object
        [ ( "uuid", Uuid.encode format.uuid )
        , ( "name", E.string format.name )
        , ( "icon", E.string format.icon )
        ]
