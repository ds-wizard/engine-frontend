module Questionnaires.Detail.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Dict exposing (Dict)
import FormEngine.Model exposing (Form)
import KnowledgeModels.Editor.Models.Entities exposing (Chapter)
import Questionnaires.Common.Models exposing (QuestionnaireDetail)


type alias Model =
    { uuid : String
    , questionnaire : ActionResult QuestionnaireDetail
    , activeChapterForm : Maybe Form
    , activeChapter : Maybe Chapter
    , values : Dict String String
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaire = Loading
    , activeChapterForm = Nothing
    , activeChapter = Nothing
    , values = Dict.empty
    , savingQuestionnaire = Unset
    }
