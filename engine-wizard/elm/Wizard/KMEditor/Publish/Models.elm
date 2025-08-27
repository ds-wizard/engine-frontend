module Wizard.KMEditor.Publish.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.KMEditor.Common.BranchPublishForm as BranchPublishForm exposing (BranchPublishForm)


type alias Model =
    { branch : ActionResult BranchDetail
    , publishingBranch : ActionResult String
    , form : Form FormError BranchPublishForm
    }


initialModel : Model
initialModel =
    { branch = Loading
    , publishingBranch = Unset
    , form = BranchPublishForm.init
    }
