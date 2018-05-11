module DSPlanner.Detail.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import DSPlanner.Common.Models exposing (QuestionnaireDetail)
import Dict exposing (Dict)
import FormEngine.Model exposing (Form)
import KMEditor.Editor.Models.Entities exposing (Chapter)


type alias Model =
    { uuid : String
    , questionnaire : ActionResult QuestionnaireDetail
    , activeChapterForm : Maybe Form
    , activeChapter : Maybe Chapter
    , replies : Dict String String
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaire = Loading
    , activeChapterForm = Nothing
    , activeChapter = Nothing
    , replies = Dict.empty
    , savingQuestionnaire = Unset
    }
