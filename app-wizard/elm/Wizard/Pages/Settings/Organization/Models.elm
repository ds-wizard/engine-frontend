module Wizard.Pages.Settings.Organization.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.OrganizationConfigForm as OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model OrganizationConfigForm


initialModel : AppState -> Model
initialModel appState =
    GenericModel.initialModel (OrganizationConfigForm.initEmpty appState)
