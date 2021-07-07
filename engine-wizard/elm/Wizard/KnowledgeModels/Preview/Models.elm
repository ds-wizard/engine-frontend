module Wizard.KnowledgeModels.Preview.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Wizard.Common.Components.Questionnaire as Questionnaire


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , package : ActionResult PackageDetail
    , questionnaireModel : ActionResult Questionnaire.Model
    , mbQuestionUuid : Maybe String
    , creatingQuestionnaire : ActionResult Questionnaire
    }


initialModel : Maybe String -> Model
initialModel mbQuestionUuid =
    { knowledgeModel = Loading
    , package = Loading
    , questionnaireModel = Loading
    , mbQuestionUuid = mbQuestionUuid
    , creatingQuestionnaire = Unset
    }
