module Wizard.Pages.Projects.FileDownload.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , fileUuid : Uuid
    , urlResponse : ActionResult ()
    }


initialModel : Uuid -> Uuid -> Model
initialModel projectUuid fileUuid =
    { projectUuid = projectUuid
    , fileUuid = fileUuid
    , urlResponse = ActionResult.Loading
    }
