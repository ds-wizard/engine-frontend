module KMEditor.Editor2.Models exposing
    ( EditorType(..)
    , Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor2.Preview.Models


type EditorType
    = KMEditor
    | TagsEditor
    | PreviewEditor
    | HistoryEditor


type alias Model =
    { branchUuid : String
    , branch : ActionResult Branch
    , metrics : ActionResult (List Metric)
    , levels : ActionResult (List Level)
    , preview : ActionResult KnowledgeModel
    , currentEditor : EditorType
    , sessionEvents : List Event
    , previewEditorModel : Maybe KMEditor.Editor2.Preview.Models.Model
    }


initialModel : String -> Model
initialModel branchUuid =
    { branchUuid = branchUuid
    , branch = Loading
    , metrics = Loading
    , levels = Loading
    , preview = Unset
    , currentEditor = KMEditor
    , sessionEvents = []
    , previewEditorModel = Nothing
    }
