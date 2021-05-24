module Wizard.KnowledgeModels.Preview.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Wizard.Common.Components.Questionnaire as Questionnaire


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , package : ActionResult PackageDetail
    , levels : ActionResult (List Level)
    , metrics : ActionResult (List Metric)
    , questionnaireModel : ActionResult Questionnaire.Model
    , mbQuestionUuid : Maybe String
    , creatingQuestionnaire : ActionResult Questionnaire
    }


initialModel : Maybe String -> Model
initialModel mbQuestionUuid =
    { knowledgeModel = Loading
    , package = Loading
    , levels = Loading
    , metrics = Loading
    , questionnaireModel = Loading
    , mbQuestionUuid = mbQuestionUuid
    , creatingQuestionnaire = Unset
    }
