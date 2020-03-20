module Wizard.Settings.Affiliation.Models exposing
    ( Model
    , initialModel
    )

import Wizard.Settings.Common.AffiliationConfigForm as AffiliationConfigForm exposing (AffiliationConfigForm)
import Wizard.Settings.Common.EditableAffiliationConfig exposing (EditableAffiliationConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    GenericModel.Model EditableAffiliationConfig AffiliationConfigForm


initialModel : Model
initialModel =
    GenericModel.initialModel AffiliationConfigForm.initEmpty
