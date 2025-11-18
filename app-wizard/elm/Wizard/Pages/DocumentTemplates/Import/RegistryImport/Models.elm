module Wizard.Pages.DocumentTemplates.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { documentTemplateId : String
    , pulling : ActionResult ()
    }


initialModel : String -> Model
initialModel documentTemplateId =
    { documentTemplateId = documentTemplateId
    , pulling = Unset
    }
