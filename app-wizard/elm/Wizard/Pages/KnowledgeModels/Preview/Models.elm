module Wizard.Pages.KnowledgeModels.Preview.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Components.Questionnaire as Questionnaire


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , knowledgeModelPackage : ActionResult KnowledgeModelPackageDetail
    , questionnaireModel : ActionResult Questionnaire.Model
    , mbQuestionUuid : Maybe String
    , creatingQuestionnaire : ActionResult Project
    }


initialModel : Maybe String -> Model
initialModel mbQuestionUuid =
    { knowledgeModel = Loading
    , knowledgeModelPackage = Loading
    , questionnaireModel = Loading
    , mbQuestionUuid = mbQuestionUuid
    , creatingQuestionnaire = Unset
    }
