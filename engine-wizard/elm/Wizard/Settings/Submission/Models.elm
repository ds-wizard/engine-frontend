module Wizard.Settings.Submission.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Shared.Data.TemplateSuggestion exposing (TemplateSuggestion)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { templates : ActionResult (List TemplateSuggestion)
    , genericModel : GenericModel.Model EditableSubmissionConfig
    }


initialModel : Model
initialModel =
    { templates = Loading
    , genericModel = GenericModel.initialModel EditableSubmissionConfig.initEmptyForm
    }
