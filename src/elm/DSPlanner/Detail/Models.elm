module DSPlanner.Detail.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import DSPlanner.Common.Models exposing (QuestionnaireDetail)
import FormEngine.Model exposing (Form, FormValues)
import KMEditor.Common.Models.Entities exposing (Chapter)


type alias Model =
    { uuid : String
    , questionnaire : ActionResult QuestionnaireDetail
    , activeChapterForm : Maybe Form
    , activeChapter : Maybe Chapter
    , replies : FormValues
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaire = Loading
    , activeChapterForm = Nothing
    , activeChapter = Nothing
    , replies = []
    , savingQuestionnaire = Unset
    }
