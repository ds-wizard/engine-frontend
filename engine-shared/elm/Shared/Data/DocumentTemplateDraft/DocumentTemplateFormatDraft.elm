module Shared.Data.DocumentTemplateDraft.DocumentTemplateFormatDraft exposing
    ( DocumentTemplateFormatDraft
    , decoder
    , encode
    , field
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.DocumentTemplate.DocumentTemplateFormatStep as DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Uuid exposing (Uuid)


type alias DocumentTemplateFormatDraft =
    { uuid : Uuid
    , name : String
    , icon : String
    , steps : List DocumentTemplateFormatStep
    }


validation : Validation FormError DocumentTemplateFormatDraft
validation =
    V.succeed DocumentTemplateFormatDraft
        |> V.andMap (V.field "uuid" V.uuid)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "icon" V.string)
        |> V.andMap (V.field "steps" (V.list DocumentTemplateFormatStep.validation))


field : DocumentTemplateFormatDraft -> Field
field format =
    Field.group
        [ ( "uuid", Field.string (Uuid.toString format.uuid) )
        , ( "name", Field.string format.name )
        , ( "icon", Field.string format.icon )
        , ( "steps", Field.list (List.map DocumentTemplateFormatStep.field format.steps) )
        ]


decoder : Decoder DocumentTemplateFormatDraft
decoder =
    D.succeed DocumentTemplateFormatDraft
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "icon" D.string
        |> D.required "steps" (D.list DocumentTemplateFormatStep.decoder)


encode : DocumentTemplateFormatDraft -> E.Value
encode draft =
    E.object
        [ ( "uuid", E.string (Uuid.toString draft.uuid) )
        , ( "name", E.string draft.name )
        , ( "icon", E.string draft.icon )
        , ( "steps", E.list DocumentTemplateFormatStep.encode draft.steps )
        ]
