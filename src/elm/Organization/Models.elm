module Organization.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Organization.Common.Organization exposing (Organization)
import Organization.Common.OrganizationForm as OrganizationForm exposing (OrganizationForm)


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
