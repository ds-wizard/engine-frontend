module Wizard.Pages.KMEditor.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.BranchCreateForm as BranchCreateForm exposing (BranchCreateForm)


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
