module Wizard.KnowledgeModels.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PackageDetail exposing (PackageDetail)


type alias Model =
    { package : ActionResult PackageDetail
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : Model
initialModel =
    { package = Loading
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
