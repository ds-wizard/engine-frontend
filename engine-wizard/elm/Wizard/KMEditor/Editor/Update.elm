module Wizard.KMEditor.Editor.Update exposing
    ( fetchData
    , isGuarded
    , onUnload
    , update
    )

import ActionResult exposing (ActionResult(..))
import Debounce
import Dict
import Gettext exposing (gettext)
import Maybe.Extra as Maybe
import Random exposing (Seed)
import Shared.Api.WebSocket as WebSocket
import Shared.Data.ApiError as ApiError
import Shared.Data.WebSockets.WebSocketServerAction as WebSocketServerAction
import Shared.Utils exposing (getUuid, getUuidString)
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Branches as BranchesApi
import Wizard.Api.KnowledgeModelSecrets as KnowledgeModelSecrets
import Wizard.Api.Models.Branch.BranchState as BranchState
import Wizard.Api.Models.Event as Event
import Wizard.Api.Models.WebSockets.BranchAction.SetContentBranchAction as SetContentBranchAction exposing (SetContentBranchAction)
import Wizard.Api.Models.WebSockets.ClientBranchAction as ClientBranchAction
import Wizard.Api.Models.WebSockets.ServerBranchAction as ServerBranchAction
import Wizard.Api.Prefabs as PrefabsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.KMEditor.Editor.Components.Settings as Settings
import Wizard.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.KMEditor.Editor.Models exposing (Model, addSavingActionUuid, getSecrets, initPageModel, removeSavingActionUuid)
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
            [ BranchesApi.getBranch appState uuid GetBranchComplete
            , PrefabsApi.getIntegrationPrefabs appState GetIntegrationPrefabsComplete
            , KnowledgeModelSecrets.getKnowledgeModelSecrets appState GetKnowledgeModelSecretsComplete
            ]


fetchSubrouteData : AppState -> Model -> Cmd Msg
fetchSubrouteData appState model =
    case appState.route of
        KMEditorRoute (EditorRoute _ (KMEditorRoute.Edit mbActiveUuid)) ->
            let
                activeEditorUuid =
                    mbActiveUuid
                        |> Maybe.withDefault Uuid.nil
                        |> Uuid.toString
            in
            Cmd.batch
                [ Ports.scrollToTop "#editor-view"
                , Ports.scrollTreeItemIntoView ("[data-km-editor-link=\"" ++ activeEditorUuid ++ "\"]")
                , Ports.focus ("[data-editor-uuid=\"" ++ activeEditorUuid ++ "\"] input")
                ]

        KMEditorRoute (EditorRoute _ KMEditorRoute.Preview) ->
            case model.branchModel of
                Success branchModel ->
                    Ports.scrollIntoView ("#question-" ++ EditorBranch.getActiveQuestionUuid branchModel)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


fetchSubrouteDataFromAfter : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
fetchSubrouteDataFromAfter wrapMsg appState model =
    case ( model.branchModel, appState.route ) of
        ( Success _, KMEditorRoute (EditorRoute _ route) ) ->
            let
                ( newSeed, newModel ) =
                    initPageModel appState route model
            in
            ( newSeed
            , newModel
            , Cmd.map wrapMsg <| fetchSubrouteData appState newModel
            )

        _ ->
            ( appState.seed, model, Cmd.none )


isGuarded : AppState -> Routes.Route -> Model -> Maybe String
isGuarded appState nextRoute model =
    if List.isEmpty model.savingActionUuids then
        Nothing

    else if Routes.isKmEditorEditor model.uuid nextRoute then
        Nothing

    else
        Just (gettext "Some changes are still saving." appState.locale)


onUnload : Routes.Route -> Model -> Cmd Msg
onUnload nextRoute model =
    let
        leaveCmd =
            Cmd.batch
                [ WebSocket.close model.websocket
                , Task.dispatch ResetModel
                ]
    in
    case nextRoute of
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
                            ( newSeed, newModel, fetchCmd ) =
                                fetchSubrouteDataFromAfter wrapMsg
                                    appState
                                    { model
                                        | branchModel = Success (EditorBranch.init appState (getSecrets model) branch model.mbEditorUuid)
                                        , settingsModel = Settings.setBranchDetail appState branch model.settingsModel
                                    }
                        in
                        ( newSeed
                        , newModel
                        , Cmd.batch
                            [ WebSocket.open model.websocket
                            , fetchCmd
                            ]
                        )

                    else
                        withSeed ( model, cmdNavigate appState (Routes.kmEditorMigration model.uuid) )

                Err error ->
                    withSeed <|
                        ( { model | branchModel = ApiError.toActionResult appState (gettext "Unable to get the knowledge model editor." appState.locale) error }
                        , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                        )

        GetIntegrationPrefabsComplete result ->
            case result of
                Ok integrationPrefabs ->
                    withSeed ( { model | integrationPrefabs = Success <| List.map .content integrationPrefabs }, Cmd.none )

                Err _ ->
                    withSeed ( { model | integrationPrefabs = Error "" }, Cmd.none )

        GetKnowledgeModelSecretsComplete result ->
            case result of
                Ok secrets ->
                    withSeed
                        ( { model
                            | kmSecrets = Success secrets
                            , branchModel = ActionResult.map (EditorBranch.computeWarnings appState (List.map .name secrets)) model.branchModel
                          }
                        , Cmd.none
                        )

                Err error ->
                    withSeed <|
                        ( { model | kmSecrets = ApiError.toActionResult appState (gettext "Unable to get knowledge model secrets." appState.locale) error }
                        , Cmd.none
                        )

        WebSocketMsg wsMsg ->
            handleWebSocketMsg wsMsg appState model

        WebSocketPing ->
            withSeed ( model, WebSocket.ping model.websocket )

        SavingMsg savingMsg ->
            withSeed ( { model | savingModel = ProjectSaving.update savingMsg model.savingModel }, Cmd.none )

        Refresh ->
            withSeed ( model, Ports.refresh () )

        KMEditorMsg kmEditorMsg ->
            case model.branchModel of
                Success branchModel ->
                    let
                        updateConfig =
                            { setFullscreenMsg = Wizard.Msgs.SetFullscreen
                            , wrapMsg = wrapMsg << KMEditorMsg
                            , eventMsg = \shouldDebounce mbFocusSelector parentUuid mbEntityUuid createEvent -> wrapMsg <| EventMsg shouldDebounce mbFocusSelector parentUuid mbEntityUuid createEvent
                            }

                        ( editorBranch, kmEditorModel, cmd ) =
                            KMEditor.update appState updateConfig kmEditorMsg ( branchModel, model.kmEditorModel )
                    in
                    withSeed ( { model | kmEditorModel = kmEditorModel, branchModel = Success editorBranch }, cmd )

                _ ->
                    withSeed ( model, Cmd.none )

        PhaseEditorMsg phaseEditorMsg ->
            let
                ( phaseEditorModel, cmd ) =
                    PhaseEditor.update phaseEditorMsg model.phaseEditorModel
            in
            withSeed ( { model | phaseEditorModel = phaseEditorModel }, Cmd.map (wrapMsg << PhaseEditorMsg) cmd )

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

        PublishModalMsg publishModalMsg ->
            let
                publishModalUpdateConfig =
                    { wrapMsg = wrapMsg << PublishModalMsg
                    , branchUuid = model.uuid
                    }

                ( publishModalModel, publishModalCmd ) =
                    PublishModal.update publishModalUpdateConfig appState publishModalMsg model.publishModalModel
            in
            withSeed ( { model | publishModalModel = publishModalModel }, publishModalCmd )

        EventMsg shouldDebounce mbFocusSelector parentUuid mbEntityUuid createEvent ->
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

                newBranchModel =
                    ActionResult.map (EditorBranch.applyEvent appState (getSecrets model) True event) model.branchModel

                setUnloadMessageCmd =
                    Ports.setUnloadMessage (gettext "Some changes are still saving." appState.locale)

                focusSelectorCmd =
                    Maybe.unwrap Cmd.none Ports.focus mbFocusSelector
            in
            if shouldDebounce then
                let
                    squashedEvent =
                        case Dict.get entityUuid model.eventsLastEvent of
                            Just lastEvent ->
                                Event.squash lastEvent event

                            Nothing ->
                                event

                    wsEvent =
                        SetContentBranchAction.AddBranchEvent
                            { uuid = wsEventUuid
                            , event = squashedEvent
                            }

                    ( debounce, debounceCmd ) =
                        Debounce.push (debounceConfig appState entityUuid)
                            wsEvent
                            (getDebounceModel entityUuid model)
                in
                ( newSeed3
                , { model
                    | branchModel = newBranchModel
                    , kmEditorModel = KMEditor.closeAllModals model.kmEditorModel
                    , eventsWebsocketDebounce = Dict.insert entityUuid debounce model.eventsWebsocketDebounce
                    , eventsLastEvent = Dict.insert entityUuid squashedEvent model.eventsLastEvent
                  }
                , Cmd.batch [ Cmd.map wrapMsg debounceCmd, setUnloadMessageCmd, focusSelectorCmd ]
                )

            else
                let
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

                    navigateCmd =
                        getNavigateCmd appState model.uuid model.branchModel newBranchModel
                in
                ( newSeed3
                , { newModel
                    | branchModel = newBranchModel
                    , kmEditorModel = KMEditor.closeAllModals model.kmEditorModel
                  }
                , Cmd.batch [ wsCmd, setUnloadMessageCmd, navigateCmd, focusSelectorCmd ]
                )

        EventDebounceMsg entityUuid debounceMsg ->
            let
                send event =
                    let
                        wsCmd =
                            event
                                |> ClientBranchAction.SetContent
                                |> ClientBranchAction.encode
                                |> WebSocket.send model.websocket
                    in
                    Cmd.batch
                        [ wsCmd
                        , Task.dispatch (EventAddSavingUuid (SetContentBranchAction.getUuid event) entityUuid)
                        ]

                ( debounce, cmd ) =
                    Debounce.update
                        (debounceConfig appState entityUuid)
                        (Debounce.takeLast send)
                        debounceMsg
                        (getDebounceModel entityUuid model)
            in
            withSeed <|
                ( { model | eventsWebsocketDebounce = Dict.insert entityUuid debounce model.eventsWebsocketDebounce }
                , Cmd.map wrapMsg cmd
                )

        SavePreviewReplies ->
            let
                ( eventUuid, newSeed ) =
                    getUuid appState.seed

                event =
                    { uuid = eventUuid
                    , replies = model.previewModel.questionnaireModel.questionnaire.replies
                    }

                wsCmd =
                    event
                        |> ClientBranchAction.SetReplies
                        |> ClientBranchAction.encode
                        |> WebSocket.send model.websocket
            in
            ( newSeed, addSavingActionUuid eventUuid model, wsCmd )

        EventAddSavingUuid eventUuid entityUuid ->
            let
                newModel =
                    { model | eventsLastEvent = Dict.remove entityUuid model.eventsLastEvent }
            in
            withSeed ( addSavingActionUuid eventUuid newModel, Cmd.none )


handleWebSocketMsg : WebSocket.RawMsg -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleWebSocketMsg websocketMsg appState model =
    let
        updateModel eventData =
            let
                ( newModel, removed ) =
                    removeSavingActionUuid eventData.uuid model

                newModel2 =
                    if not removed then
                        { newModel | branchModel = ActionResult.map (EditorBranch.applyEvent appState (getSecrets model) False eventData.event) newModel.branchModel }

                    else
                        newModel

                clearUnloadMessageCmd =
                    if removed && List.isEmpty newModel2.savingActionUuids then
                        Ports.clearUnloadMessage ()

                    else
                        Cmd.none

                navigateCmd =
                    getNavigateCmd appState model.uuid model.branchModel newModel2.branchModel
            in
            ( appState.seed
            , newModel2
            , Cmd.batch [ clearUnloadMessageCmd, navigateCmd ]
            )
    in
    case WebSocket.receive ServerBranchAction.decoder websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerBranchAction.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = users }, Cmd.none )

                        ServerBranchAction.SetContent setContentBranchAction ->
                            case setContentBranchAction of
                                SetContentBranchAction.AddBranchEvent data ->
                                    updateModel data

                        ServerBranchAction.SetReplies event ->
                            let
                                ( newModel, _ ) =
                                    removeSavingActionUuid event.uuid model
                            in
                            ( appState.seed
                            , { newModel
                                | branchModel = ActionResult.map (EditorBranch.setReplies event.replies) newModel.branchModel
                                , previewModel = Preview.setReplies event.replies newModel.previewModel
                              }
                            , Cmd.none
                            )

                WebSocketServerAction.Error _ ->
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


debounceConfig : AppState -> String -> Debounce.Config Msg
debounceConfig appState entityUuid =
    { strategy = Debounce.soon appState.websocketThrottleDelay
    , transform = EventDebounceMsg entityUuid
    }


getDebounceModel : String -> Model -> Debounce.Debounce SetContentBranchAction
getDebounceModel entityUuid model =
    Maybe.withDefault Debounce.init (Dict.get entityUuid model.eventsWebsocketDebounce)
