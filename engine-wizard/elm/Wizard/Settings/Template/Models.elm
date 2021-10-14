module Wizard.Settings.Template.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { genericModel : GenericModel.Model TemplateConfig
    , templates : ActionResult (List TemplateSuggestion)
    }


initialModel : Model
initialModel =
    { genericModel = GenericModel.initialModel TemplateConfig.initEmptyForm
    , templates = Loading
    }
