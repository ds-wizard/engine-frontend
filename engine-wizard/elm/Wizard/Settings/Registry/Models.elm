module Wizard.Settings.Registry.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.EditableConfig.EditableRegistryConfig as EditableRegistryConfig exposing (EditableRegistryConfig)
import Shared.Form.FormError exposing (FormError)
import Wizard.Settings.Common.Forms.RegistrySignupForm as RegistrySignupForm exposing (RegistrySignupForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model EditableRegistryConfig
    , registrySignupOpen : Bool
    , registrySigningUp : ActionResult String
    , registrySignupForm : Form FormError RegistrySignupForm
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel EditableRegistryConfig.initEmptyForm
    , registrySignupOpen = False
    , registrySigningUp = Unset
    , registrySignupForm = RegistrySignupForm.initEmpty
    }
