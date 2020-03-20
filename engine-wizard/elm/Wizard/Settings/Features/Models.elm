module Wizard.Settings.Features.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Common.FeaturesConfigForm as FeaturesConfigForm exposing (FeaturesConfigForm)


type alias Model =
    { config : ActionResult EditableFeaturesConfig
    , savingConfig : ActionResult String
    , form : Form CustomFormError FeaturesConfigForm
    }


initialModel : Model
initialModel =
    { config = Loading
    , savingConfig = Unset
    , form = FeaturesConfigForm.initEmpty
    }
