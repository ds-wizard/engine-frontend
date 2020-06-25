module Wizard.Documents.Common.DocumentCreateForm exposing
    ( DocumentCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)


type alias DocumentCreateForm =
    { name : String
    , questionnaireUuid : String
    , templateId : String
    , formatUuid : String
    }


init : Maybe Uuid -> Form FormError DocumentCreateForm
init mbQuestionniareUuid =
    let
        initialFields =
            case mbQuestionniareUuid of
                Just questionnaireUuid ->
                    [ ( "questionnaireUuid", Field.string (Uuid.toString questionnaireUuid) ) ]

                Nothing ->
                    []
    in
    Form.initial initialFields validation


validation : Validation FormError DocumentCreateForm
validation =
    Validate.map4 DocumentCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "questionnaireUuid" Validate.string)
        (Validate.field "templateId" Validate.string)
        (Validate.field "formatUuid" Validate.string)


encode : DocumentCreateForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string form.questionnaireUuid )
        , ( "templateId", E.string form.templateId )
        , ( "formatUuid", E.string form.formatUuid )
        ]
