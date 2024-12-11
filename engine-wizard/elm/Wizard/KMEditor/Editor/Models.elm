module Wizard.KMEditor.Editor.Models exposing
    ( Model
    , addSavingActionUuid
    , init
    , initPageModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Random exposing (Seed)
import Shared.Api.Branches as BranchesApi
import Shared.Data.Event exposing (Event)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.OnlineUserInfo exposing (OnlineUserInfo)
import Shared.Data.WebSockets.BranchAction.SetContentBranchAction exposing (SetContentBranchAction)
import Shared.WebSocket as WebSocket exposing (WebSocket)
import String.Extra as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.KMEditor.Editor.Components.Settings as Settings
import Wizard.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute)
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving


type alias Model =
    { uuid : Uuid
    , mbEditorUuid : Maybe Uuid
    , websocket : WebSocket
    , offline : Bool
    , error : Bool
    , onlineUsers : List OnlineUserInfo
    , savingActionUuids : List Uuid
    , savingModel : ProjectSaving.Model
    , branchModel : ActionResult EditorBranch
    , kmEditorModel : KMEditor.Model
    , phaseEditorModel : PhaseEditor.Model
    , tagEditorModel : TagEditor.Model
    , previewModel : Preview.Model
    , settingsModel : Settings.Model
    , integrationPrefabs : ActionResult (List Integration)
    , publishModalModel : PublishModal.Model
    , eventsLastEvent : Dict String Event
    , eventsWebsocketDebounce : Dict String (Debounce SetContentBranchAction)
    }


init : AppState -> Uuid -> Maybe Uuid -> Model
init appState uuid mbEditorUuid =
    { uuid = uuid
    , mbEditorUuid = mbEditorUuid
    , websocket = WebSocket.init (BranchesApi.websocket uuid appState)
    , offline = False
    , error = False
    , onlineUsers = []
    , savingActionUuids = []
    , savingModel = ProjectSaving.init
    , branchModel = ActionResult.Loading
    , kmEditorModel = KMEditor.initialModel
    , phaseEditorModel = PhaseEditor.initialModel
    , tagEditorModel = TagEditor.initialModel
    , previewModel = Preview.initialModel appState ""
    , settingsModel = Settings.initialModel
    , integrationPrefabs = ActionResult.Loading
    , publishModalModel = PublishModal.initialModel
    , eventsLastEvent = Dict.empty
    , eventsWebsocketDebounce = Dict.empty
    }


initPageModel : AppState -> KMEditorRoute -> Model -> ( Seed, Model )
initPageModel appState route model =
    case route of
        KMEditorRoute.Edit mbEditorUuid ->
            ( appState.seed
            , { model | branchModel = ActionResult.map (EditorBranch.setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)) model.branchModel }
            )

        KMEditorRoute.Preview ->
            case model.branchModel of
                ActionResult.Success editorBranch ->
                    let
                        currentQuestionUuid =
                            EditorBranch.getActiveQuestionUuid editorBranch

                        packageId =
                            ActionResult.map .branch model.branchModel
                                |> ActionResult.toMaybe
                                |> Maybe.andThen .previousPackageId
                                |> Maybe.withDefault ""

                        firstChapterUuid =
                            editorBranch.branch.knowledgeModel.chapterUuids
                                |> EditorBranch.filterDeleted editorBranch
                                |> List.head
                                |> Maybe.withDefault ""

                        activeChapterUuid =
                            EditorBranch.getChapterUuid editorBranch.activeUuid editorBranch

                        selectedChapterUuid =
                            String.withDefault firstChapterUuid activeChapterUuid

                        defaultPhaseUuid =
                            List.head editorBranch.branch.knowledgeModel.phaseUuids
                                |> Maybe.andThen Uuid.fromString

                        ( newSeed, previewModel ) =
                            model.previewModel
                                |> Preview.setPackageId appState packageId
                                |> Preview.setReplies editorBranch.branch.replies
                                |> Preview.generateReplies appState currentQuestionUuid editorBranch.branch.knowledgeModel
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
