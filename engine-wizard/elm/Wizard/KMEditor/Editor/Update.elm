module Wizard.KMEditor.Editor.Update exposing (fetchData, onUnload, update)

import ActionResult exposing (ActionResult(..))
import List.Extra as List
import Random exposing (Seed)
import Shared.Api.Branches as BranchesApi
import Shared.Api.Prefabs as PrefabsApi
import Shared.Data.Branch.BranchState as BranchState
import Shared.Data.WebSockets.BranchAction.SetContentBranchAction as SetContentBranchAction
import Shared.Data.WebSockets.ClientBranchAction as ClientBranchAction
import Shared.Data.WebSockets.ServerBranchAction as ServerBranchAction
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (lg)
import Shared.Utils exposing (dispatch, getUuid, getUuidString)
import Shared.WebSocket as WebSocket
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.Components.Settings as Settings
import Wizard.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Editor.Models exposing (Model, addSavingActionUuid, initPageModel, removeSavingActionUuid)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Model -> Cmd Msg
fetchData appState uuid model =
    if ActionResult.unwrap False (.branch >> .uuid >> (==) uuid) model.branchModel then
        Cmd.batch
            [ fetchSubrouteData appState model
            , WebSocket.open model.websocket
            ]

    else
        Cmd.batch
            [ BranchesApi.getBranch uuid appState GetBranchComplete
            , PrefabsApi.getIntegrationPrefabs appState GetIntegrationPrefabsComplete
            ]


fetchSubrouteData : AppState -> model -> Cmd Msg
fetchSubrouteData appState _ =
    case appState.route of
        KMEditorRoute (EditorRoute _ (KMEditorRoute.Edit _)) ->
            Ports.scrollToTop "#editor-view"

        _ ->
            Cmd.none


fetchSubrouteDataFromAfter : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
fetchSubrouteDataFromAfter wrapMsg appState model =
    case ( model.branchModel, appState.route ) of
        ( Success _, KMEditorRoute (EditorRoute _ route) ) ->
            ( initPageModel appState route model, Cmd.map wrapMsg <| fetchSubrouteData appState model )

        _ ->
            ( model, Cmd.none )


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload newRoute model =
    let
        leaveCmd =
            Cmd.batch
                [ WebSocket.close model.websocket
                , dispatch ResetModel
                ]
    in
    case newRoute of
        KMEditorRoute (EditorRoute uuid _) ->
            if uuid == model.uuid then
                Cmd.none

            else
                leaveCmd

        _ ->
            leaveCmd


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    let
        withSeed ( m, c ) =
            ( appState.seed, m, c )
    in
    case msg of
        ResetModel ->
            ( appState.seed, { model | uuid = Uuid.nil }, Cmd.none )

        GetBranchComplete result ->
            case result of
                Ok branch ->
                    if BranchState.isEditable branch.state then
                        let
                            ( newModel, fetchCmd ) =
                                fetchSubrouteDataFromAfter wrapMsg
                                    appState
                                    { model
                                        | branchModel = Success (EditorBranch.init branch model.mbEditorUuid)
                                        , settingsModel = Settings.setBranchDetail branch model.settingsModel
                                    }
                        in
                        withSeed
                            ( newModel
                            , Cmd.batch
                                [ WebSocket.open model.websocket
                                , fetchCmd
                                ]
                            )

                    else
                        withSeed ( model, cmdNavigate appState (Routes.kmEditorMigration model.uuid) )

                Err error ->
                    withSeed <|
                        ( { model | branchModel = ApiError.toActionResult appState (lg "apiError.branches.getError" appState) error }
                        , getResultCmd result
                        )

        GetIntegrationPrefabsComplete result ->
            case result of
                Ok integrationPrefabs ->
                    withSeed ( { model | integrationPrefabs = Success <| List.map .content integrationPrefabs }, Cmd.none )

                Err _ ->
                    withSeed ( { model | integrationPrefabs = Error "" }, Cmd.none )

        WebSocketMsg wsMsg ->
            handleWebSocketMsg wsMsg appState model

        WebSocketPing _ ->
            withSeed ( model, WebSocket.ping model.websocket )

        OnlineUserMsg index onlineUserMsg ->
            withSeed ( { model | onlineUsers = List.updateAt index (OnlineUser.update onlineUserMsg) model.onlineUsers }, Cmd.none )

        SavingMsg savingMsg ->
            withSeed ( { model | savingModel = ProjectSaving.update savingMsg model.savingModel }, Cmd.none )

        Refresh ->
            withSeed ( model, Ports.refresh () )

        KMEditorMsg kmEditorMsg ->
            case model.branchModel of
                Success branchModel ->
                    let
                        ( editorBranch, kmEditorModel, cmd ) =
                            KMEditor.update Wizard.Msgs.SetFullscreen kmEditorMsg model.kmEditorModel branchModel
                    in
                    withSeed ( { model | kmEditorModel = kmEditorModel, branchModel = Success editorBranch }, cmd )

                _ ->
                    withSeed ( model, Cmd.none )

        TagEditorMsg tagEditorMsg ->
            let
                ( tagEditorModel, cmd ) =
                    TagEditor.update tagEditorMsg model.tagEditorModel
            in
            withSeed ( { model | tagEditorModel = tagEditorModel }, Cmd.map (wrapMsg << TagEditorMsg) cmd )

        PreviewMsg previewMsg ->
            case model.branchModel of
                Success editorBranch ->
                    let
                        ( newSeed, previewModel, cmd ) =
                            Preview.update previewMsg appState editorBranch model.previewModel
                    in
                    ( newSeed, { model | previewModel = previewModel }, Cmd.map (wrapMsg << PreviewMsg) cmd )

                _ ->
                    withSeed ( model, Cmd.none )

        SettingsMsg settingsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << SettingsMsg
                    , cmdNavigate = cmdNavigate
                    , branchUuid = model.uuid
                    }

                ( settingsModel, cmd ) =
                    Settings.update updateConfig appState settingsMsg model.settingsModel
            in
            withSeed ( { model | settingsModel = settingsModel }, cmd )

        EventMsg parentUuid mbEntityUuid createEvent ->
            let
                ( kmEventUuid, newSeed1 ) =
                    getUuidString appState.seed

                ( wsEventUuid, newSeed2 ) =
                    getUuid newSeed1

                ( entityUuid, newSeed3 ) =
                    case mbEntityUuid of
                        Just eUuid ->
                            ( eUuid, newSeed2 )

                        Nothing ->
                            getUuidString newSeed2

                event =
                    createEvent
                        { uuid = kmEventUuid
                        , parentUuid = parentUuid
                        , entityUuid = entityUuid
                        , createdAt = appState.currentTime
                        }

                wsEvent =
                    SetContentBranchAction.AddBranchEvent
                        { uuid = wsEventUuid
                        , event = event
                        }

                newModel =
                    addSavingActionUuid wsEventUuid model

                wsCmd =
                    wsEvent
                        |> ClientBranchAction.SetContent
                        |> ClientBranchAction.encode
                        |> WebSocket.send model.websocket

                newBranchModel =
                    ActionResult.map (EditorBranch.applyEvent True event) model.branchModel

                navigateCmd =
                    getNavigateCmd appState model.uuid model.branchModel newBranchModel
            in
            ( newSeed3
            , { newModel
                | branchModel = newBranchModel
                , kmEditorModel = KMEditor.closeAllModals model.kmEditorModel
              }
            , Cmd.batch [ wsCmd, navigateCmd ]
            )


handleWebSocketMsg : WebSocket.RawMsg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleWebSocketMsg websocketMsg appState model =
    let
        updateModel eventData =
            let
                ( newModel, removed ) =
                    removeSavingActionUuid eventData.uuid model

                newModel2 =
                    if not removed then
                        { newModel | branchModel = ActionResult.map (EditorBranch.applyEvent False eventData.event) newModel.branchModel }

                    else
                        newModel

                navigateCmd =
                    getNavigateCmd appState model.uuid model.branchModel newModel2.branchModel
            in
            ( appState.seed
            , newModel2
            , navigateCmd
            )
    in
    case WebSocket.receive ServerBranchAction.decoder websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerBranchAction.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = List.map OnlineUser.init users }, Cmd.none )

                        ServerBranchAction.SetContent setContentBranchAction ->
                            case setContentBranchAction of
                                SetContentBranchAction.AddBranchEvent data ->
                                    updateModel data

                WebSocketServerAction.Error ->
                    ( appState.seed, { model | error = True }, Cmd.none )

        WebSocket.Close ->
            ( appState.seed, { model | offline = True }, Cmd.none )

        _ ->
            ( appState.seed, model, Cmd.none )


getNavigateCmd : AppState -> Uuid -> ActionResult EditorBranch -> ActionResult EditorBranch -> Cmd msg
getNavigateCmd appState uuid oldEditorBranch newEditorBranch =
    let
        originalActiveEditor =
            ActionResult.unwrap "" .activeUuid oldEditorBranch

        newActiveEditor =
            ActionResult.unwrap "" .activeUuid newEditorBranch
    in
    if not (String.isEmpty newActiveEditor) && originalActiveEditor /= newActiveEditor then
        let
            activeEditorUuid =
                if newActiveEditor == Uuid.toString Uuid.nil then
                    Nothing

                else
                    Uuid.fromString newActiveEditor
        in
        cmdNavigate appState (Routes.kmEditorEditor uuid activeEditorUuid)

    else
        Cmd.none
