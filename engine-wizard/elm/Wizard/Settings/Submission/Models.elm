module Wizard.Settings.Submission.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Settings.Common.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)
import Wizard.Settings.Generic.Model as GenericModel


type alias Model =
    { templates : ActionResult (List Template)
    , genericModel : GenericModel.Model EditableSubmissionConfig
    }


initialModel : Model
initialModel =
    { templates = Loading
    , genericModel = GenericModel.initialModel EditableSubmissionConfig.initEmptyForm
    }
