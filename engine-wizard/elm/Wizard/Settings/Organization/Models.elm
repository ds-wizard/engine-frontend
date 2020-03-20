module Wizard.Settings.Organization.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Settings.Common.EditableOrganizationConfig exposing (EditableOrganizationConfig)
import Wizard.Settings.Common.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableOrganizationConfig OrganizationConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel OrganizationConfigForm.initEmpty
