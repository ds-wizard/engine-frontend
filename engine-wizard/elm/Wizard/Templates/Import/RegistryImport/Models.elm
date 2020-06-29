module Wizard.Templates.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { templateId : String
    , pulling : ActionResult ()
    }


initialModel : String -> Model
initialModel packageId =
    { templateId = packageId
    , pulling = Unset
    }
