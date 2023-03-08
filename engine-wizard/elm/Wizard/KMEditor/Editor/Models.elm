module Wizard.KMEditor.Editor.Models exposing
    ( Model
    , addSavingActionUuid
    , init
    , initPageModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult)
import Shared.Api.Branches as BranchesApi
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.OnlineUserInfo exposing (OnlineUserInfo)
import Shared.WebSocket as WebSocket exposing (WebSocket)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
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
    }


initPageModel : AppState -> KMEditorRoute -> Model -> Model
initPageModel appState route model =
    case route of
        KMEditorRoute.Edit mbEditorUuid ->
            { model | branchModel = ActionResult.map (EditorBranch.setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)) model.branchModel }

        KMEditorRoute.Preview ->
            let
                packageId =
                    ActionResult.map .branch model.branchModel
                        |> ActionResult.toMaybe
                        |> Maybe.andThen .previousPackageId
                        |> Maybe.withDefault ""

                firstChapterUuid =
                    model.branchModel
                        |> ActionResult.unwrap Nothing (.branch >> .knowledgeModel >> .chapterUuids >> List.head)
                        |> Maybe.withDefault ""

                defaultPhaseUuid =
                    model.branchModel
                        |> ActionResult.unwrap Nothing (.branch >> .knowledgeModel >> .phaseUuids >> List.head)
                        |> Maybe.andThen Uuid.fromString

                previewModel =
                    model.previewModel
                        |> Preview.setPackageId appState packageId
                        |> Preview.setActiveChapterIfNot firstChapterUuid
                        |> Preview.setPhase defaultPhaseUuid
            in
            { model | previewModel = previewModel }

        _ ->
            model


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
