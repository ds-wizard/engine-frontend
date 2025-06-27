module Wizard.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing
    ( DocumentTemplateFormatStep
    , decoder
    , encode
    , field
    , validation
    )

import Dict exposing (Dict)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias DocumentTemplateFormatStep =
    { name : String
    , options : Dict String String
    }


decoder : Decoder DocumentTemplateFormatStep
decoder =
    D.succeed DocumentTemplateFormatStep
        |> D.required "name" D.string
        |> D.required "options" (D.dict D.string)


encode : DocumentTemplateFormatStep -> E.Value
encode format =
    E.object
        [ ( "name", E.string format.name )
        , ( "options", E.dict identity E.string format.options )
        ]


field : DocumentTemplateFormatStep -> Field
field step =
    Field.group
        [ ( "name", Field.string step.name )
        , ( "options", Field.dict Field.string step.options )
        ]


validation : Validation FormError DocumentTemplateFormatStep
validation =
    V.succeed DocumentTemplateFormatStep
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "options" (V.dict V.string))
