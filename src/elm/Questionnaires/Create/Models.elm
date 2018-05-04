module Questionnaires.Create.Models exposing (..)

import Common.Form exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import PackageManagement.Models exposing (PackageDetail)


type alias Model =
    { packages : ActionResult (List PackageDetail)
    , savingQuestionnaire : ActionResult String
    , form : Form CustomFormError QuestionnaireCreateForm
    }


initialModel : Model
initialModel =
    { packages = Loading
    , savingQuestionnaire = Unset
    , form = initQuestionnaireCreateForm
    }


type alias QuestionnaireCreateForm =
    { name : String
    , packageId : String
    }


initQuestionnaireCreateForm : Form CustomFormError QuestionnaireCreateForm
initQuestionnaireCreateForm =
    Form.initial [] questionnaireCreateFormValidation


questionnaireCreateFormValidation : Validation CustomFormError QuestionnaireCreateForm
questionnaireCreateFormValidation =
    Validate.map2 QuestionnaireCreateForm
        (Validate.field "name" Validate.string)
        (Validate.field "packageId" Validate.string)


encodeQuestionnaireCreateForm : QuestionnaireCreateForm -> Encode.Value
encodeQuestionnaireCreateForm form =
    Encode.object
        [ ( "name", Encode.string form.name )
        , ( "packageId", Encode.string form.packageId )
        ]
