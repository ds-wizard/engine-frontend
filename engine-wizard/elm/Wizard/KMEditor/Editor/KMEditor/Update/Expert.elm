module Wizard.KMEditor.Editor.KMEditor.Update.Expert exposing
    ( deleteExpert
    , removeExpert
    , updateExpertForm
    , updateIfQuestion
    , withGenerateExpertEditEvent
    )

import Form
import Random exposing (Seed)
import Shared.Locale exposing (l)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Models.Children as Children exposing (Children)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), ExpertEditorData, QuestionEditorData, isExpertEditorDirty, updateExpertEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (expertFormValidation)
import Wizard.KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import Wizard.KMEditor.Editor.KMEditor.Update.Events exposing (createAddExpertEvent, createDeleteExpertEvent, createEditExpertEvent)
import Wizard.Msgs


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.Update.Expert"


updateExpertForm : Model -> Form.Msg -> ExpertEditorData -> Model
updateExpertForm =
    updateForm
        { formValidation = expertFormValidation
        , createEditor = ExpertEditor
        }


withGenerateExpertEditEvent : AppState -> Seed -> Model -> ExpertEditorData -> (Seed -> Model -> ExpertEditorData -> ( Seed, Model, Cmd Wizard.Msgs.Msg )) -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
withGenerateExpertEditEvent appState =
    withGenerateEvent
        { isDirty = isExpertEditorDirty
        , formValidation = expertFormValidation
        , createEditor = ExpertEditor
        , alert = l_ "alert" appState
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
