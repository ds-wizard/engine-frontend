module Wizard.Settings.Submission.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.DocumentTemplateSuggestion exposing (DocumentTemplateSuggestion)
import Shared.Data.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { templates : ActionResult (List DocumentTemplateSuggestion)
    , genericModel : GenericModel.Model EditableSubmissionConfig
    }


initialModel : Model
initialModel =
    { templates = Loading
    , genericModel = GenericModel.initialModel EditableSubmissionConfig.initEmptyForm
    }
