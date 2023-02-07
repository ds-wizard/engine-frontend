module Wizard.DocumentTemplates.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.DocumentTemplateDetail exposing (DocumentTemplateDetail)


type alias Model =
    { template : ActionResult DocumentTemplateDetail
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : Model
initialModel =
    { template = Loading
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
