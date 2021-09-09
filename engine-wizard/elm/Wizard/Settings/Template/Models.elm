module Wizard.Settings.Template.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)
import Shared.Data.Template exposing (Template)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model TemplateConfig
    , templates : ActionResult (List Template)
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel TemplateConfig.initEmptyForm
    , templates = Loading
    }
