module Wizard.KMEditor.Editor.Models exposing
    ( Model
    , addSavingActionUuid
    , init
    , initPageModel
    , removeSavingActionUuid
    )

import ActionResult exposing (ActionResult)
import Shared.Api.Branches as BranchesApi
import Shared.WebSocket as WebSocket exposing (WebSocket)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.Components.Settings as Settings
import Wizard.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute)
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving


type alias Model =
    { uuid : Uuid
    , mbEditorUuid : Maybe Uuid
    , websocket : WebSocket
    , offline : Bool
    , error : Bool
    , onlineUsers : List OnlineUser.Model
    , savingActionUuids : List Uuid
    , savingModel : PlanSaving.Model
    , branchModel : ActionResult EditorBranch
    , kmEditorModel : KMEditor.Model
    , tagEditorModel : TagEditor.Model
    , previewModel : Preview.Model
    , settingsModel : Settings.Model
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
    , savingModel = PlanSaving.init
    , branchModel = ActionResult.Loading
    , kmEditorModel = KMEditor.initialModel
    , tagEditorModel = TagEditor.initialModel
    , previewModel = Preview.initialModel appState ""
    , settingsModel = Settings.initialModel
    }


initPageModel : KMEditorRoute -> Model -> Model
initPageModel route model =
    case route of
        KMEditorRoute.Edit mbEditorUuid ->
            { model | branchModel = ActionResult.map (EditorBranch.setActiveEditor (Maybe.map Uuid.toString mbEditorUuid)) model.branchModel }

        KMEditorRoute.Preview ->
            let
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
        , savingModel = PlanSaving.setSaving model.savingModel
    }


removeSavingActionUuid : Uuid -> Model -> ( Model, Bool )
removeSavingActionUuid uuid model =
    let
        newSavingActionUuids =
            List.filter ((/=) uuid) model.savingActionUuids

        newSavingModel =
            if not (List.isEmpty model.savingActionUuids) && List.isEmpty newSavingActionUuids then
                PlanSaving.setSaved model.savingModel

            else
                model.savingModel
    in
    ( { model | savingActionUuids = newSavingActionUuids, savingModel = newSavingModel }
    , List.length model.savingActionUuids /= List.length newSavingActionUuids
    )
