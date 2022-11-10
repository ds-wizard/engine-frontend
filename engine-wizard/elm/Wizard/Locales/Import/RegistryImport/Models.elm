module Wizard.Locales.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))


type alias Model =
    { localeId : String
    , pulling : ActionResult ()
    }


initialModel : String -> Model
initialModel localeId =
    { localeId = localeId
    , pulling = Unset
    }
