module Wizard.KMEditor.Publish.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import String
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.KMEditor.Common.BranchDetail exposing (BranchDetail)
import Wizard.KMEditor.Common.BranchPublishForm as BranchPublishForm exposing (BranchPublishForm)


type alias Model =
    { branch : ActionResult BranchDetail
    , publishingBranch : ActionResult String
    , form : Form CustomFormError BranchPublishForm
    }


initialModel : Model
initialModel =
    { branch = Loading
    , publishingBranch = Unset
    , form = BranchPublishForm.init
    }
