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
    , private : Bool
    }


initQuestionnaireEditForm : QuestionnaireDetail -> Form CustomFormError QuestionnaireEditForm
initQuestionnaireEditForm questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) questionnaireEditFormValidation


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    [ ( "name", Field.string questionnaire.name )
    , ( "private", Field.bool questionnaire.private )
    ]


questionnaireEditFormValidation : Validation CustomFormError QuestionnaireEditForm
questionnaireEditFormValidation =
    Validate.map2 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "private" Validate.bool)


encodeEditForm : QuestionnaireDetail -> QuestionnaireEditForm -> Encode.Value
encodeEditForm questionnaire form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "private", Encode.bool form.private )
        , ( "replies", encodeFormValues questionnaire.replies )
        , ( "level", Encode.int questionnaire.level )
        ]
