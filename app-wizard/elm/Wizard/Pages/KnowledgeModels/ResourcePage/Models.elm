module Wizard.Pages.KnowledgeModels.ResourcePage.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)


type alias Model =
    { knowledgeModel : ActionResult KnowledgeModel
    , resourcePageUuid : String
    }


initialModel : String -> Model
initialModel resourcePageUuid =
    { knowledgeModel = ActionResult.Loading
    , resourcePageUuid = resourcePageUuid
    }
