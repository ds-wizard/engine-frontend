module Questionnaires.Detail.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import KMEditor.Common.Models.Entities exposing (Level, Metric)


type alias Model =
    { uuid : String
    , questionnaireModel : ActionResult Common.Questionnaire.Models.Model
    , questionnaireDetail : ActionResult QuestionnaireDetail
    , levels : ActionResult (List Level)
    , metrics : ActionResult (List Metric)
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaireModel = Loading
    , questionnaireDetail = Loading
    , levels = Loading
    , metrics = Loading
    , savingQuestionnaire = Unset
    }
