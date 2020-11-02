module Wizard.KMEditor.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.KMEditor.Common.BranchCreateForm as BranchCreateForm exposing (BranchCreateForm)


type alias Model =
    { savingBranch : ActionResult ()
    , form : Form FormError BranchCreateForm
    , packageTypeHintInputModel : TypeHintInput.Model PackageSuggestion
    , selectedPackage : Maybe String
    }


initialModel : Maybe String -> Model
initialModel selectedPackage =
    { savingBranch = Unset
    , form = BranchCreateForm.init selectedPackage
    , packageTypeHintInputModel = TypeHintInput.init "previousPackageId"
    , selectedPackage = selectedPackage
    }
