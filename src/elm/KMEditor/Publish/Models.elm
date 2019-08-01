module KMEditor.Publish.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import KMEditor.Common.BranchDetail exposing (BranchDetail)
import KMEditor.Common.BranchPublishForm as BranchPublishForm exposing (BranchPublishForm)
import String


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
