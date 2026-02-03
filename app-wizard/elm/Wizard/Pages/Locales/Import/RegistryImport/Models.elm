module Wizard.Pages.Locales.Import.RegistryImport.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.Api.Models.LocaleInfo exposing (LocaleInfo)


type alias Model =
    { localeId : String
    , locale : ActionResult LocaleInfo
    }


initialModel : String -> Model
initialModel localeId =
    { localeId = localeId
    , locale = Unset
    }
