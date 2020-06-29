module Wizard.Templates.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.TemplateDetail exposing (TemplateDetail)


type alias Model =
    { template : ActionResult TemplateDetail
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : Model
initialModel =
    { template = Loading
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
