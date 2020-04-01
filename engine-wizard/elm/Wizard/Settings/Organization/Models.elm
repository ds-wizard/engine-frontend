module Wizard.Settings.Organization.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Settings.Common.Forms.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model OrganizationConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel OrganizationConfigForm.initEmpty
