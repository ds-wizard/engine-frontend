module Wizard.Projects.Common.QuestionnaireFromTemplateCreateForm exposing
    ( QuestionnaireFromTemplateCreateForm
    , encode
    , init
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type alias QuestionnaireFromTemplateCreateForm =
    { name : String
    , questionnaireUuid : String
    }


init : Maybe String -> Form FormError QuestionnaireFromTemplateCreateForm
init mbSelectedQuestionnaireUuid =
    let
        initials =
            case mbSelectedQuestionnaireUuid of
                Just questionnaireUuid ->
                    [ ( "questionnaireUuid", Field.string questionnaireUuid ) ]

                _ ->
                    []
    in
    Form.initial initials validation


validation : Validation FormError QuestionnaireFromTemplateCreateForm
validation =
    Validate.map2 QuestionnaireFromTemplateCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "questionnaireUuid" Validate.string)


encode : QuestionnaireFromTemplateCreateForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "questionnaireUuid", E.string form.questionnaireUuid )
        ]
