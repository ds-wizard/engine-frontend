module Wizard.Pages.DocumentTemplates.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Api.Models.UuidResponse exposing (UuidResponse)


type alias Model =
    { documentTemplateId : String
    , pulling : ActionResult UuidResponse
    }


initialModel : String -> Model
initialModel documentTemplateId =
    { documentTemplateId = documentTemplateId
    , pulling = Unset
    }
