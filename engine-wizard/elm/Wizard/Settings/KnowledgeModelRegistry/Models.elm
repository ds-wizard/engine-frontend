module Wizard.Settings.KnowledgeModelRegistry.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableKnowledgeModelRegistryConfig as EditableKnowledgeModelRegistryConfig exposing (EditableKnowledgeModelRegistryConfig)
import Wizard.Settings.Common.Forms.RegistrySignupForm as RegistrySignupForm exposing (RegistrySignupForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model EditableKnowledgeModelRegistryConfig
    , registrySignupOpen : Bool
    , registrySigningUp : ActionResult String
    , registrySignupForm : Form CustomFormError RegistrySignupForm
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel EditableKnowledgeModelRegistryConfig.initEmptyForm
    , registrySignupOpen = False
    , registrySigningUp = Unset
    , registrySignupForm = RegistrySignupForm.initEmpty
    }
