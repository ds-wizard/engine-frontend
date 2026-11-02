module Common.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing
    ( DocumentTemplateFormatStep
    , decoder
    , encode
    , field
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


{-| One step of a format, and what it is given to work with.

The options are pairs rather than a `Dict`, because they are edited: a dictionary
has no order, so a row would move as its name was typed, and two rows whose names
are both still empty would be one row. They are sent as the object the server
keeps them in.

-}
type alias DocumentTemplateFormatStep =
    { name : String
    , options : List ( String, String )
    }


decoder : Decoder DocumentTemplateFormatStep
decoder =
    D.succeed DocumentTemplateFormatStep
        |> D.required "name" D.string
        |> D.required "options" optionsDecoder


{-| An object has no order to read, so the pairs are put in one: by name, which
is the order a dictionary gave them before. What is typed keeps the order it was
typed in until the draft is loaded again.
-}
optionsDecoder : Decoder (List ( String, String ))
optionsDecoder =
    D.map (List.sortBy Tuple.first) (D.keyValuePairs D.string)


encode : DocumentTemplateFormatStep -> E.Value
encode format =
    E.object
        [ ( "name", E.string format.name )
        , ( "options", E.object (List.map (Tuple.mapSecond E.string) format.options) )
        ]


field : DocumentTemplateFormatStep -> Field
field step =
    Field.group
        [ ( "name", Field.string step.name )
        , ( "options", Field.list (List.map optionField step.options) )
        ]


optionField : ( String, String ) -> Field
optionField ( key, value ) =
    Field.group [ ( "key", Field.string key ), ( "value", Field.string value ) ]


validation : Validation FormError DocumentTemplateFormatStep
validation =
    V.succeed DocumentTemplateFormatStep
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "options" (V.list optionValidation))


optionValidation : Validation FormError ( String, String )
optionValidation =
    V.succeed Tuple.pair
        |> V.andMap (V.field "key" V.string)
        |> V.andMap (V.field "value" V.string)
