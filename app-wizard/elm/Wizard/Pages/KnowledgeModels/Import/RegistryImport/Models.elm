module Wizard.Pages.KnowledgeModels.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.UuidResponse exposing (UuidResponse)


type alias Model =
    { knowledgeModelPackageId : String
    , pulling : ActionResult UuidResponse
    }


initialModel : String -> Model
initialModel kmPackageId =
    { knowledgeModelPackageId = kmPackageId
    , pulling = Unset
    }
