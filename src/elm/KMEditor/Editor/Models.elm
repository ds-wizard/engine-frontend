module KMEditor.Editor.Models exposing
    ( EditorType(..)
    , Model
    , addSessionEvents
    , containsChanges
    , getSavingError
    , hasSavingError
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import KMEditor.Common.BranchDetail exposing (BranchDetail)
import KMEditor.Common.Events.Event exposing (Event)
import KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KMEditor.Editor.KMEditor.Models as KMEditorModel
import KMEditor.Editor.Preview.Models
import KMEditor.Editor.TagEditor.Models as TagEditorModel


type EditorType
    = KMEditor
    | TagsEditor
    | PreviewEditor
    | HistoryEditor


type alias Model =
    { kmUuid : String
    , km : ActionResult BranchDetail
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
initialModel kmUuid =
    { kmUuid = kmUuid
    , km = Loading
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


addSessionEvents : List Event -> Model -> Model
addSessionEvents events model =
    { model | sessionEvents = model.sessionEvents ++ events }


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
