module Wizard.KMEditor.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.KMEditor.Common.BranchCreateForm as BranchCreateForm exposing (BranchCreateForm)


type alias Model =
    { savingBranch : ActionResult ()
    , form : Form FormError BranchCreateForm
    , packageTypeHintInputModel : TypeHintInput.Model PackageSuggestion
    , package : ActionResult PackageDetail
    , selectedPackage : Maybe String
    , edit : Bool
    }


initialModel : AppState -> Maybe String -> Maybe Bool -> Model
initialModel appState selectedPackage edit =
    { savingBranch = Unset
    , form = BranchCreateForm.init appState selectedPackage
    , packageTypeHintInputModel = TypeHintInput.init "previousPackageId"
    , package = ActionResult.Loading
    , selectedPackage = selectedPackage
    , edit = Maybe.withDefault False edit
    }
