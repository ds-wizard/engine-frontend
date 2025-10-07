module Wizard.Pages.Settings.Submission.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Wizard.Api.Models.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Wizard.Api.Models.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Wizard.Pages.Settings.Generic.Model as GenericModel


type alias Model =
    { templates : ActionResult (List DocumentTemplateSuggestion)
    , genericModel : GenericModel.Model EditableSubmissionConfig
    }


initialModel : Model
initialModel =
    { templates = Loading
    , genericModel = GenericModel.initialModel EditableSubmissionConfig.initEmptyForm
    }
