module Wizard.Documents.Create.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Template exposing (Template)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    , templates : ActionResult (List Template)
    , form : Form FormError DocumentCreateForm
    , savingDocument : ActionResult String
    , lastFetchedTemplatesFor : Maybe Uuid
    , selectedQuestionnaire : Maybe Uuid
    }


initialModel : Maybe Uuid -> Model
initialModel selected =
    { questionnaires = Loading
    , templates = Unset
    , form = DocumentCreateForm.init selected
    , savingDocument = Unset
    , lastFetchedTemplatesFor = Nothing
    , selectedQuestionnaire = selected
    }
