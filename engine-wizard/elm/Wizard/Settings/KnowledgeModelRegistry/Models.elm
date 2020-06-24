module Wizard.Settings.KnowledgeModelRegistry.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.EditableConfig.EditableKnowledgeModelRegistryConfig as EditableKnowledgeModelRegistryConfig exposing (EditableKnowledgeModelRegistryConfig)
import Shared.Form.FormError exposing (FormError)
import Wizard.Settings.Common.Forms.RegistrySignupForm as RegistrySignupForm exposing (RegistrySignupForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model EditableKnowledgeModelRegistryConfig
    , registrySignupOpen : Bool
    , registrySigningUp : ActionResult String
    , registrySignupForm : Form FormError RegistrySignupForm
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel EditableKnowledgeModelRegistryConfig.initEmptyForm
    , registrySignupOpen = False
    , registrySigningUp = Unset
    , registrySignupForm = RegistrySignupForm.initEmpty
    }
