module KMEditor.Editor.Models exposing
    ( EditorType(..)
    , Model
    , containsChanges
    , getSavingError
    , hasSavingError
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor.KMEditor.Models as KMEditorModel
import KMEditor.Editor.Preview.Models
import KMEditor.Editor.TagEditor.Models as TagEditorModel


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
    , previewEditorModel : Maybe KMEditor.Editor.Preview.Models.Model
    , tagEditorModel : Maybe TagEditorModel.Model
    , editorModel : Maybe KMEditorModel.Model
    , saving : ActionResult String
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
    , tagEditorModel = Nothing
    , editorModel = Nothing
    , saving = Unset
    }


containsChanges : Model -> Bool
containsChanges model =
    let
        tagEditorDirty =
            model.tagEditorModel
                |> Maybe.map TagEditorModel.containsChanges
                |> Maybe.withDefault False

        kmEditorDirty =
            model.editorModel
                |> Maybe.map KMEditorModel.containsChanges
                |> Maybe.withDefault False
    in
    List.length model.sessionEvents > 0 || tagEditorDirty || kmEditorDirty


hasSavingError : Model -> Bool
hasSavingError =
    .saving >> ActionResult.isError


getSavingError : Model -> String
getSavingError model =
    case model.saving of
        Error err ->
            err

        _ ->
            ""
