module Wizard.Organization.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Organization.Common.Organization exposing (Organization)
import Wizard.Organization.Common.OrganizationForm as OrganizationForm exposing (OrganizationForm)


type alias Model =
    { organization : ActionResult Organization
    , savingOrganization : ActionResult String
    , form : Form CustomFormError OrganizationForm
    }


initialModel : Model
initialModel =
    { organization = Loading
    , savingOrganization = Unset
    , form = OrganizationForm.initEmpty
    }
