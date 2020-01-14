module Wizard.KnowledgeModels.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.KnowledgeModels.Common.Package exposing (Package)


type alias Model =
    { packageId : String
    , pulling : ActionResult ()
    }


initialModel : String -> Model
initialModel packageId =
    { packageId = packageId
    , pulling = Unset
    }
