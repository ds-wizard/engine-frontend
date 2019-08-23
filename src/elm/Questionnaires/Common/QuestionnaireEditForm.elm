module Questionnaires.Common.QuestionnaireEditForm exposing
    ( QuestionnaireEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Common.Form exposing (CustomFormError)
import Common.FormEngine.Model exposing (encodeFormValues)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccesibility exposing (QuestionnaireAccessibility)
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireLabel as QuestionnaireLabel


type alias QuestionnaireEditForm =
    { name : String
    , accessibility : QuestionnaireAccessibility
    }


initEmpty : Form CustomFormError QuestionnaireEditForm
initEmpty =
    Form.initial [] validation


init : QuestionnaireDetail -> Form CustomFormError QuestionnaireEditForm
init questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) validation


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    [ ( "name", Field.string questionnaire.name )
    , ( "accessibility", Field.string <| QuestionnaireAccesibility.toString questionnaire.accessibility )
    ]


validation : Validation CustomFormError QuestionnaireEditForm
validation =
    Validate.map2 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "accessibility" QuestionnaireAccesibility.validation)


encode : QuestionnaireDetail -> QuestionnaireEditForm -> E.Value
encode questionnaire form =
    E.object
        [ ( "name", E.string form.name )
        , ( "accessibility", QuestionnaireAccesibility.encode form.accessibility )
        , ( "replies", encodeFormValues questionnaire.replies )
        , ( "level", E.int questionnaire.level )
        , ( "labels", E.list QuestionnaireLabel.encode questionnaire.labels )
        ]
