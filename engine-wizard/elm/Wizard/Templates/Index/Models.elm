module Wizard.Templates.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.Package exposing (Package)
import Shared.Data.Template exposing (Template)
import Wizard.Common.Components.Listing as Listing


type alias Model =
    { templates : ActionResult (Listing.Model Template)
    , templateToBeDeleted : Maybe Template
    , deletingTemplate : ActionResult String
    }


initialModel : Model
initialModel =
    { templates = Loading
    , templateToBeDeleted = Nothing
    , deletingTemplate = Unset
    }
