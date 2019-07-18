module Questionnaires.Index.ExportModal.Models exposing
    ( Model
    , initialModel
    , setQuestionnaire
    )

import ActionResult exposing (ActionResult(..))
import Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Questionnaires.Common.Template exposing (Template)


type alias Model =
    { questionnaire : Maybe Questionnaire
    , templates : ActionResult (List Template)
    , selectedFormat : String
    , selectedTemplate : Maybe String
    }


initialModel : Model
initialModel =
    { questionnaire = Nothing
    , templates = Unset
    , selectedFormat = "pdf"
    , selectedTemplate = Nothing
    }


setQuestionnaire : Questionnaire -> Model -> Model
setQuestionnaire questionnaire model =
    { model
        | questionnaire = Just questionnaire
        , templates = Loading
    }
