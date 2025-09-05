module Wizard.Pages.Settings.Registry.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.EditableConfig.EditableRegistryConfig as EditableRegistryConfig exposing (EditableRegistryConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.RegistrySignupForm as RegistrySignupForm exposing (RegistrySignupForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model EditableRegistryConfig
    , registrySignupOpen : Bool
    , registrySigningUp : ActionResult String
    , registrySignupForm : Form FormError RegistrySignupForm
    }


initialModel : AppState -> Model
initialModel appState =
    { genericModel = GenericModel.initialModel EditableRegistryConfig.initEmptyForm
    , registrySignupOpen = False
    , registrySigningUp = Unset
    , registrySignupForm = RegistrySignupForm.initEmpty appState
    }
