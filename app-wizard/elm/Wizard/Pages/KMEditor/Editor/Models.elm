module Wizard.Pages.KMEditor.Editor.Models exposing
    ( Model
    , addSavingActionUuid
    , getSecrets
    , init
    , initPageModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult)
import Common.Api.WebSocket as WebSocket exposing (WebSocket)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Random exposing (Seed)
import String.Extra as String
import Uuid exposing (Uuid)
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.Models.Event exposing (Event)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Api.Models.OnlineUserInfo exposing (OnlineUserInfo)
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetContentKnowledgeModelEditorAction exposing (SetContentKnowledgeModelEditorAction)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)
import Wizard.Pages.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.Pages.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.Pages.KMEditor.Editor.Components.Preview as Preview
import Wizard.Pages.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.KMEditor.Editor.Components.Settings as Settings
import Wizard.Pages.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute)
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving


type alias Model =
    { uuid : Uuid
    , mbEditorUuid : Maybe Uuid
    , websocket : WebSocket
    , offline : Bool
    , error : Bool
    , onlineUsers : List OnlineUserInfo
    , savingActionUuids : List Uuid
    , savingModel : ProjectSaving.Model
    , editorContext : ActionResult EditorContext
    , kmEditorModel : KMEditor.Model
    , phaseEditorModel : PhaseEditor.Model
    , tagEditorModel : TagEditor.Model
    , previewModel : Preview.Model
    , settingsModel : Settings.Model
    , integrationPrefabs : ActionResult (List Integration)
    , kmSecrets : ActionResult (List KnowledgeModelSecret)
    , publishModalModel : PublishModal.Model
    , eventsLastEvent : Dict String Event
    , eventsWebsocketDebounce : Dict String (Debounce SetContentKnowledgeModelEditorAction)
    , warningsDebounce : Debounce ()
    }


init : AppState -> Uuid -> Maybe Uuid -> Model
init appState uuid mbEditorUuid =
    { uuid = uuid
    , mbEditorUuid = mbEditorUuid
    , websocket = WebSocket.init (KnowledgeModelEditorsApi.websocket appState uuid)
    , offline = False
    , error = False
    , onlineUsers = []
    , savingActionUuids = []
    , savingModel = ProjectSaving.init
    , editorContext = ActionResult.Loading
    , kmEditorModel = KMEditor.initialModel
    , phaseEditorModel = PhaseEditor.initialModel
    , tagEditorModel = TagEditor.initialModel
    , previewModel = Preview.initialModel appState ""
    , settingsModel = Settings.initialModel appState
    , integrationPrefabs = ActionResult.Loading
    , kmSecrets = ActionResult.Loading
    , publishModalModel = PublishModal.initialModel
    , eventsLastEvent = Dict.empty
    , eventsWebsocketDebounce = Dict.empty
    , warningsDebounce = Debounce.init
    }


initPageModel : AppState -> KMEditorRoute -> Model -> ( Seed, Model )
initPageModel appState route model =
    case route of
        KMEditorRoute.Edit mbEditorUuid ->
            ( appState.seed
            , { model | editorContext = ActionResult.map (EditorContext.setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)) model.editorContext }
            )

        KMEditorRoute.Preview ->
            case model.editorContext of
                ActionResult.Success editorContext ->
                    let
                        currentQuestionUuid =
                            EditorContext.getActiveQuestionUuid editorContext

                        kmPackageId =
                            ActionResult.map .kmEditor model.editorContext
                                |> ActionResult.toMaybe
                                |> Maybe.andThen .previousPackageId
                                |> Maybe.withDefault ""

                        firstChapterUuid =
                            editorContext.kmEditor.knowledgeModel.chapterUuids
                                |> EditorContext.filterDeleted editorContext
                                |> List.head
                                |> Maybe.withDefault ""

                        activeChapterUuid =
                            EditorContext.getChapterUuid editorContext.activeUuid editorContext

                        selectedChapterUuid =
                            String.withDefault firstChapterUuid activeChapterUuid

                        defaultPhaseUuid =
                            List.head editorContext.kmEditor.knowledgeModel.phaseUuids
                                |> Maybe.andThen Uuid.fromString

                        ( newSeed, previewModel ) =
                            model.previewModel
                                |> Preview.setKnowledgeModelPackageId appState kmPackageId
                                |> Preview.setKnowledgeModel (EditorContext.getFilteredKM editorContext)
                                |> Preview.setReplies editorContext.kmEditor.replies
                                |> Preview.generateReplies appState currentQuestionUuid editorContext.kmEditor.knowledgeModel
                                |> Tuple.mapSecond (Preview.setActiveChapterIfNot selectedChapterUuid)
                                |> Tuple.mapSecond (Preview.setPhase defaultPhaseUuid)
                    in
                    ( newSeed, { model | previewModel = previewModel } )

                _ ->
                    ( appState.seed, model )

        _ ->
            ( appState.seed, model )


addSavingActionUuid : Uuid -> Model -> Model
addSavingActionUuid uuid model =
    { model
        | savingActionUuids = uuid :: model.savingActionUuids
        , savingModel = ProjectSaving.setSaving model.savingModel
    }


removeSavingActionUuid : Uuid -> Model -> ( Model, Bool )
removeSavingActionUuid uuid model =
    let
        newSavingActionUuids =
            List.filter ((/=) uuid) model.savingActionUuids

        newSavingModel =
            if not (List.isEmpty model.savingActionUuids) && List.isEmpty newSavingActionUuids then
                ProjectSaving.setSaved model.savingModel

            else
                model.savingModel
    in
    ( { model | savingActionUuids = newSavingActionUuids, savingModel = newSavingModel }
    , List.length model.savingActionUuids /= List.length newSavingActionUuids
    )


getSecrets : Model -> List String
getSecrets model =
    ActionResult.unwrap [] (List.map .name) model.kmSecrets
