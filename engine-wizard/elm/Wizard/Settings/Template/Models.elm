module Wizard.Settings.Template.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)
import Shared.Data.Template exposing (Template)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model TemplateConfig
    , templates : ActionResult (List Template)
    }


initialModel =
    { genericModel = GenericModel.initialModel TemplateConfig.initEmptyForm
    , templates = Loading
    }
