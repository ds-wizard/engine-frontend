module Wizard.Documents.Create.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Documents.Common.DocumentCreateForm as DocumentCreateForm exposing (DocumentCreateForm)
import Wizard.Documents.Common.Template exposing (Template)
import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)


type alias Model =
    { questionnaires : ActionResult (List Questionnaire)
    , templates : ActionResult (List Template)
    , form : Form CustomFormError DocumentCreateForm
    , savingDocument : ActionResult String
    , lastFetchedTemplatesFor : Maybe String
    , selectedQuestionnaire : Maybe String
    }


initialModel : Maybe String -> Model
initialModel selected =
    { questionnaires = Loading
    , templates = Unset
    , form = DocumentCreateForm.init selected
    , savingDocument = Unset
    , lastFetchedTemplatesFor = Nothing
    , selectedQuestionnaire = selected
    }
