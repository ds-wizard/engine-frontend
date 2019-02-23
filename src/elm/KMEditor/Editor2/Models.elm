module KMEditor.Editor2.Models exposing
    ( EditorType(..)
    , Model
    , applyCurrentEditorChanges
    , containsChanges
    , getSavingError
    , hasSavingError
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Editor2.Preview.Models
import KMEditor.Editor2.TagEditor.Models as TagEditorModel
import Random exposing (Seed)


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
    , tagEditorModel : Maybe TagEditorModel.Model
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
    , saving = Unset
    }


containsChanges : Model -> Bool
containsChanges model =
    let
        tagEditorDirty =
            model.tagEditorModel
                |> Maybe.map TagEditorModel.containsChanges
                |> Maybe.withDefault False
    in
    List.length model.sessionEvents > 0 || tagEditorDirty


applyCurrentEditorChanges : Seed -> Model -> ( Seed, Model )
applyCurrentEditorChanges seed model =
    let
        ( newSeed, newEvents ) =
            case ( model.currentEditor, model.preview ) of
                ( TagsEditor, Success km ) ->
                    model.tagEditorModel
                        |> Maybe.map (TagEditorModel.generateEvents seed km)
                        |> Maybe.withDefault ( seed, [] )

                _ ->
                    ( seed, [] )
    in
    ( newSeed, { model | sessionEvents = model.sessionEvents ++ newEvents } )


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
