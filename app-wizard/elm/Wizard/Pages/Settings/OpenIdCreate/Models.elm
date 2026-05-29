module Wizard.Pages.Settings.OpenIdCreate.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.OpenIdClientForm as OpenIdClientForm exposing (OpenIdClientForm)


type alias Model =
    { form : Form FormError OpenIdClientForm
    , savingForm : ActionResult ()
    , openIdPrefabs : ActionResult (List OpenIdClientDetail)
    , advancedConfigExpanded : Bool
    }


initialModel : AppState -> Model
initialModel appState =
    { form = OpenIdClientForm.initEmpty appState
    , savingForm = ActionResult.Unset
    , openIdPrefabs = ActionResult.Loading
    , advancedConfigExpanded = False
    }
