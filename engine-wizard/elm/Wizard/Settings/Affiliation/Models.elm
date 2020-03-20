module Wizard.Settings.Affiliation.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.AffiliationConfigForm as AffiliationConfigForm exposing (AffiliationConfigForm)
import Wizard.Settings.Common.EditableAffiliationConfig exposing (EditableAffiliationConfig)


type alias Model =
    { config : ActionResult EditableAffiliationConfig
    , savingConfig : ActionResult String
    , form : Form CustomFormError AffiliationConfigForm
    }


initialModel : Model
initialModel =
    { config = Loading
    , savingConfig = Unset
    , form = AffiliationConfigForm.initEmpty
    }
