module Wizard.Documents.Create.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Template exposing (Template)
import Shared.Form.FormError exposing (FormError)
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)


type alias Model =
    { questionnaire : ActionResult QuestionnaireDetail
    , templates : ActionResult (List Template)
    , form : Form FormError DocumentCreateForm
    , savingDocument : ActionResult String
    }


initialModel : Model
initialModel =
    { questionnaire = Loading
    , templates = Unset
    , form = DocumentCreateForm.init
    , savingDocument = Unset
    }
