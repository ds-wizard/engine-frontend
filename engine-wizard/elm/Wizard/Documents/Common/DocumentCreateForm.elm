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
import Wizard.Common.Form exposing (CustomFormError)


type alias DocumentCreateForm =
    { name : String
    , questionnaireUuid : String
    , templateUuid : String
    , format : String
    }


init : Maybe String -> Form CustomFormError DocumentCreateForm
init mbQuestionniareUuid =
    let
        initialFields =
            case mbQuestionniareUuid of
                Just questionnaireUuid ->
                    [ ( "questionnaireUuid", Field.string questionnaireUuid ) ]

                Nothing ->
                    []
    in
    Form.initial initialFields validation


validation : Validation CustomFormError DocumentCreateForm
validation =
    Validate.map4 DocumentCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "questionnaireUuid" Validate.string)
        (Validate.field "templateUuid" Validate.string)
        (Validate.field "format" Validate.string)


encode : DocumentCreateForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string form.questionnaireUuid )
        , ( "templateUuid", E.string form.templateUuid )
        , ( "format", E.string form.format )
        ]
