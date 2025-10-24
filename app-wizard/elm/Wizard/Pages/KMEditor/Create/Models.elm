module Wizard.Pages.KMEditor.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorCreateForm as KnowledgeModelEditorCreateForm exposing (KnowledgeModelEditorCreateForm)


type alias Model =
    { savingKmEditor : ActionResult ()
    , form : Form FormError KnowledgeModelEditorCreateForm
    , packageTypeHintInputModel : TypeHintInput.Model PackageSuggestion
    , package : ActionResult PackageDetail
    , selectedPackage : Maybe String
    , edit : Bool
    }


initialModel : AppState -> Maybe String -> Maybe Bool -> Model
initialModel appState selectedPackage edit =
    { savingKmEditor = Unset
    , form = KnowledgeModelEditorCreateForm.init appState selectedPackage
    , packageTypeHintInputModel = TypeHintInput.init "previousPackageId"
    , package = ActionResult.Loading
    , selectedPackage = selectedPackage
    , edit = Maybe.withDefault False edit
    }
