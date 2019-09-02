module KMEditor.Editor.KMEditor.Update.Integration exposing (deleteIntegration, removeIntegration, updateIntegrationForm, withGenerateIntegrationEditEvent)

import Common.AppState exposing (AppState)
import Common.Locale exposing (l)
import Form
import KMEditor.Editor.KMEditor.Models exposing (Model, getCurrentIntegrations)
import KMEditor.Editor.KMEditor.Models.Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), IntegrationEditorData, KMEditorData, isIntegrationEditorDirty, updateIntegrationEditorData)
import KMEditor.Editor.KMEditor.Models.Forms exposing (integrationFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddIntegrationEvent, createDeleteIntegrationEvent, createEditIntegrationEvent)
import Msgs
import Random exposing (Seed)


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.KMEditor.Update.Integration"


updateIntegrationForm : Model -> Form.Msg -> IntegrationEditorData -> Model
updateIntegrationForm model formMsg editorData =
    updateForm
        { formValidation = integrationFormValidation (getCurrentIntegrations model) editorData.uuid
        , createEditor = IntegrationEditor
        }
        model
        formMsg
        editorData


withGenerateIntegrationEditEvent :
    AppState
    -> Seed
    -> Model
    -> IntegrationEditorData
    -> (Seed -> Model -> IntegrationEditorData -> ( Seed, Model, Cmd Msgs.Msg ))
    -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateIntegrationEditEvent appState seed model editorData =
    withGenerateEvent
        { isDirty = isIntegrationEditorDirty
        , formValidation = integrationFormValidation (getCurrentIntegrations model) editorData.uuid
        , createEditor = IntegrationEditor
        , alert = l_ "alert" appState
        , createAddEvent = createAddIntegrationEvent
        , createEditEvent = createEditIntegrationEvent
        , updateEditorData = updateIntegrationEditorData
        , updateEditors = Nothing
        }
        seed
        model
        editorData


deleteIntegration : Seed -> Model -> String -> IntegrationEditorData -> ( Seed, Model )
deleteIntegration =
    deleteEntity
        { removeEntity = removeIntegration
        , createEditor = IntegrationEditor
        , createDeleteEvent = createDeleteIntegrationEvent
        }


removeIntegration : (String -> Children -> Children) -> String -> Editor -> Editor
removeIntegration removeFn uuid =
    updateIfKMEditor (\data -> { data | integrations = removeFn uuid data.integrations })


updateIfKMEditor : (KMEditorData -> KMEditorData) -> Editor -> Editor
updateIfKMEditor update editor =
    case editor of
        KMEditor kmEditorData ->
            KMEditor <| update kmEditorData

        _ ->
            editor
