module Questionnaires.Detail.Models exposing (..)

import Common.Types exposing (ActionResult(Loading))
import FormEngine.Model exposing (Form)
import KnowledgeModels.Editor.Models.Entities exposing (Chapter)
import Questionnaires.Common.Models exposing (QuestionnaireDetail)


type alias Model =
    { questionnaire : ActionResult QuestionnaireDetail
    , activeChapterForm : Maybe Form
    , activeChapter : Maybe Chapter
    }


initialModel : Model
initialModel =
    { questionnaire = Loading
    , activeChapterForm = Nothing
    , activeChapter = Nothing
    }
