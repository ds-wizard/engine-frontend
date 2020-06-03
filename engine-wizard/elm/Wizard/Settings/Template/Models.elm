module Wizard.Settings.Template.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Config.TemplateConfig as TemplateConfig exposing (TemplateConfig)
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model TemplateConfig
    , templates : ActionResult (List Template)
    }


initialModel =
    { genericModel = GenericModel.initialModel TemplateConfig.initEmptyForm
    , templates = Loading
    }
