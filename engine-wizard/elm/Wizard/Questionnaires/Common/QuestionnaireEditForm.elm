module Wizard.Questionnaires.Common.QuestionnaireEditForm exposing
    ( QuestionnaireEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.FormEngine.Model exposing (encodeFormValues)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireLabel as QuestionnaireLabel
import Wizard.Questionnaires.Common.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias QuestionnaireEditForm =
    { name : String
    , visibility : QuestionnaireVisibility
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
    , ( "visibility", Field.string <| QuestionnaireVisibility.toString questionnaire.visibility )
    ]


validation : Validation CustomFormError QuestionnaireEditForm
validation =
    Validate.map2 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "visibility" QuestionnaireVisibility.validation)


encode : QuestionnaireDetail -> QuestionnaireEditForm -> E.Value
encode questionnaire form =
    E.object
        [ ( "name", E.string form.name )
        , ( "visibility", QuestionnaireVisibility.encode form.visibility )
        , ( "replies", encodeFormValues questionnaire.replies )
        , ( "level", E.int questionnaire.level )
        , ( "labels", E.list QuestionnaireLabel.encode questionnaire.labels )
        ]
