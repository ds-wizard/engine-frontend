module Wizard.Pages.KMEditor.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorCreateForm as KnowledgeModelEditorCreateForm exposing (KnowledgeModelEditorCreateForm)


type alias Model =
    { savingKmEditor : ActionResult ()
    , form : Form FormError KnowledgeModelEditorCreateForm
    , kmPackageTypeHintInputModel : TypeHintInput.Model KnowledgeModelPackageSuggestion
    , kmPackage : ActionResult KnowledgeModelPackageDetail
    , selectedKmPackage : Maybe String
    , edit : Bool
    }


initialModel : AppState -> Maybe String -> Maybe Bool -> Model
initialModel appState selectedPackage edit =
    { savingKmEditor = Unset
    , form = KnowledgeModelEditorCreateForm.init appState selectedPackage
    , kmPackageTypeHintInputModel = TypeHintInput.init "previousKnowledgeModelPackageId"
    , kmPackage = ActionResult.Loading
    , selectedKmPackage = selectedPackage
    , edit = Maybe.withDefault False edit
    }
