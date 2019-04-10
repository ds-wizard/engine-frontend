module KMEditor.Editor.KMEditor.Update.Integration exposing (deleteIntegration, removeIntegration, updateIntegrationForm, withGenerateIntegrationEditEvent)

import Form
import KMEditor.Editor.KMEditor.Models exposing (Model)
import KMEditor.Editor.KMEditor.Models.Children exposing (Children)
import KMEditor.Editor.KMEditor.Models.Editors exposing (Editor(..), IntegrationEditorData, KMEditorData, isIntegrationEditorDirty, updateIntegrationEditorData)
import KMEditor.Editor.KMEditor.Models.Forms exposing (integrationFormValidation)
import KMEditor.Editor.KMEditor.Update.Abstract exposing (deleteEntity, updateForm, withGenerateEvent)
import KMEditor.Editor.KMEditor.Update.Events exposing (createAddIntegrationEvent, createDeleteIntegrationEvent, createEditIntegrationEvent)
import Msgs
import Random exposing (Seed)


updateIntegrationForm : Model -> Form.Msg -> IntegrationEditorData -> Model
updateIntegrationForm =
    updateForm
        { formValidation = integrationFormValidation
        , createEditor = IntegrationEditor
        }


withGenerateIntegrationEditEvent :
    Seed
    -> Model
    -> IntegrationEditorData
    -> (Seed -> Model -> IntegrationEditorData -> ( Seed, Model, Cmd Msgs.Msg ))
    -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateIntegrationEditEvent =
    withGenerateEvent
        { isDirty = isIntegrationEditorDirty
        , formValidation = integrationFormValidation
        , createEditor = IntegrationEditor
        , alert = "Please fix the integration errors first"
        , createAddEvent = createAddIntegrationEvent
        , createEditEvent = createEditIntegrationEvent
        , updateEditorData = updateIntegrationEditorData
        , updateEditors = Nothing
        }


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
