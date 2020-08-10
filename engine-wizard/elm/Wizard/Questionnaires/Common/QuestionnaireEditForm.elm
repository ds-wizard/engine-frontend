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
import Shared.Data.Questionnaire.QuestionnaireLabel as QuestionnaireLabel
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.FormValue as FormValue
import Shared.Form.FormError exposing (FormError)


type alias QuestionnaireEditForm =
    { name : String
    , visibility : QuestionnaireVisibility
    , sharing : QuestionnaireSharing
    }


initEmpty : Form FormError QuestionnaireEditForm
initEmpty =
    Form.initial [] validation


init : QuestionnaireDetail -> Form FormError QuestionnaireEditForm
init questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) validation


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    [ ( "name", Field.string questionnaire.name )
    , ( "visibility", QuestionnaireVisibility.field questionnaire.visibility )
    , ( "sharing", QuestionnaireSharing.field questionnaire.sharing )
    ]


validation : Validation FormError QuestionnaireEditForm
validation =
    Validate.map3 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "visibility" QuestionnaireVisibility.validation)
        (Validate.field "sharing" QuestionnaireSharing.validation)


encode : QuestionnaireEditForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "visibility", QuestionnaireVisibility.encode form.visibility )
        , ( "sharing", QuestionnaireSharing.encode form.sharing )
        ]
