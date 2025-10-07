module Wizard.Pages.Projects.DocumentDownload.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Uuid exposing (Uuid)


type alias Model =
    { questionnaireUuid : Uuid
    , fileUuid : Uuid
    , urlResponse : ActionResult ()
    }


initialModel : Uuid -> Uuid -> Model
initialModel questionnaireUuid fileUuid =
    { questionnaireUuid = questionnaireUuid
    , fileUuid = fileUuid
    , urlResponse = ActionResult.Loading
    }
