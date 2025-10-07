module Wizard.Pages.KMEditor.Publish.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Pages.KMEditor.Common.BranchPublishForm as BranchPublishForm exposing (BranchPublishForm)


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
