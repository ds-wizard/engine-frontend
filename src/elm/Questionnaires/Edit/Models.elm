module Questionnaires.Edit.Models exposing
    ( Model
    , QuestionnaireEditForm
    , encodeEditForm
    , initQuestionnaireEditForm
    , initialModel
    , questionnaireEditFormValidation
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import FormEngine.Model exposing (encodeFormValues)
import Json.Encode as Encode
import Questionnaires.Common.QuestionnaireAccessibility as QuestionnaireAccesibility exposing (QuestionnaireAccessibility)


type alias Model =
    { uuid : String
    , questionnaire : ActionResult QuestionnaireDetail
    , editForm : Form CustomFormError QuestionnaireEditForm
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaire = Loading
    , editForm = Form.initial [] questionnaireEditFormValidation
    , savingQuestionnaire = Unset
    }


type alias QuestionnaireEditForm =
    { name : String
    , accessibility : QuestionnaireAccessibility
    }


initQuestionnaireEditForm : QuestionnaireDetail -> Form CustomFormError QuestionnaireEditForm
initQuestionnaireEditForm questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) questionnaireEditFormValidation


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    [ ( "name", Field.string questionnaire.name )
    , ( "accessibility", Field.string <| QuestionnaireAccesibility.toString questionnaire.accessibility )
    ]


questionnaireEditFormValidation : Validation CustomFormError QuestionnaireEditForm
questionnaireEditFormValidation =
    Validate.map2 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "accessibility" QuestionnaireAccesibility.validation)


encodeEditForm : QuestionnaireDetail -> QuestionnaireEditForm -> Encode.Value
encodeEditForm questionnaire form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "accessibility", QuestionnaireAccesibility.encode form.accessibility )
        , ( "replies", encodeFormValues questionnaire.replies )
        , ( "level", Encode.int questionnaire.level )
        ]
