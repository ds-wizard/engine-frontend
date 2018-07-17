module KMEditor.Editor.Update.Expert exposing (..)

import Form
import KMEditor.Editor.Models exposing (Model)
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.Editors exposing (Editor(ExpertEditor, QuestionEditor), ExpertEditorData, QuestionEditorData, isExpertEditorDirty, updateExpertEditorData)
import KMEditor.Editor.Models.Forms exposing (expertFormValidation)
import KMEditor.Editor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.Update.Events exposing (createAddExpertEvent, createDeleteExpertEvent, createEditExpertEvent)
import Msgs
import Random.Pcg exposing (Seed)


updateExpertForm : Model -> Form.Msg -> ExpertEditorData -> Model
updateExpertForm =
    updateForm
        { formValidation = expertFormValidation
        , createEditor = ExpertEditor
        }


withGenerateExpertEditEvent : Seed -> Model -> ExpertEditorData -> (Seed -> Model -> ExpertEditorData -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateExpertEditEvent =
    withGenerateEvent
        { isDirty = isExpertEditorDirty
        , formValidation = expertFormValidation
        , createEditor = ExpertEditor
        , alert = "Please fix the expert errors first."
        , createAddEvent = createAddExpertEvent
        , createEditEvent = createEditExpertEvent
        , updateEditorData = updateExpertEditorData
        , updateEditors = Nothing
        }


deleteExpert : Seed -> Model -> String -> ExpertEditorData -> ( Seed, Model )
deleteExpert =
    deleteEntity
        { removeEntity = removeExpert
        , createEditor = ExpertEditor
        , createDeleteEvent = createDeleteExpertEvent
        }


removeExpert : (String -> Children -> Children) -> String -> Editor -> Editor
removeExpert removeFn uuid =
    updateIfQuestion (\data -> { data | experts = removeFn uuid data.experts })


updateIfQuestion : (QuestionEditorData -> QuestionEditorData) -> Editor -> Editor
updateIfQuestion update editor =
    case editor of
        QuestionEditor kmEditorData ->
            QuestionEditor <| update kmEditorData

        _ ->
            editor
