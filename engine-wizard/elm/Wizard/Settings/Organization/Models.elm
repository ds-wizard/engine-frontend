module Wizard.Settings.Organization.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableOrganizationConfig exposing (EditableOrganizationConfig)
import Wizard.Settings.Common.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)


type alias Model =
    { config : ActionResult EditableOrganizationConfig
    , savingConfig : ActionResult String
    , form : Form CustomFormError OrganizationConfigForm
    }


initialModel : Model
initialModel =
    { config = Loading
    , savingConfig = Unset
    , form = OrganizationConfigForm.initEmpty
    }
