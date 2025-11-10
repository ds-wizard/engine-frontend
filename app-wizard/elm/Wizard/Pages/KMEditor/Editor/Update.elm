module Wizard.Pages.KMEditor.Editor.Update exposing
    ( fetchData
    , isGuarded
    , onUnload
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError
import Common.Api.Models.WebSockets.WebSocketServerAction as WebSocketServerAction
import Common.Api.WebSocket as WebSocket
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Utils.RequestHelpers as RequestHelpers
import Debounce
import Dict
import Gettext exposing (gettext)
import Random exposing (Seed)
import Task.Extra as Task
import Uuid exposing (Uuid)
import Uuid.Extra as Uuid
import Wizard.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Wizard.Api.KnowledgeModelSecrets as KnowledgeModelSecrets
import Wizard.Api.Models.Event as Event
import Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState
import Wizard.Api.Models.WebSockets.ClientKnowledgeModelEditorAction as ClientKnowledgeModelEditorAction
import Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetContentKnowledgeModelEditorAction as SetContentKnowledgeModelEditorAction exposing (SetContentKnowledgeModelEditorAction)
import Wizard.Api.Models.WebSockets.ServerKnowledgeModelEditorAction as ServerKnowledgeModelEditorAction
import Wizard.Api.Prefabs as PrefabsApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)
import Wizard.Pages.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.Pages.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.Pages.KMEditor.Editor.Components.Preview as Preview
import Wizard.Pages.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.KMEditor.Editor.Components.Settings as Settings
import Wizard.Pages.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute
import Wizard.Pages.KMEditor.Editor.Models exposing (Model, addSavingActionUuid, getSecrets, initPageModel, removeSavingActionUuid)
import Wizard.Pages.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.Pages.KMEditor.Routes exposing (Route(..))
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Routes as Routes exposing (Route(..))
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Uuid -> Model -> Cmd Msg
fetchData appState uuid model =
    if ActionResult.unwrap False (.kmEditor >> .uuid >> (==) uuid) model.editorContext then
        Cmd.batch
            [ fetchSubrouteData appState model
            , WebSocket.open model.websocket
            ]

    else
        Cmd.batch
            [ KnowledgeModelEditorsApi.getKnowledgeModelEditor appState uuid GetKnowledgeModelEditorComplete
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
                [ Dom.scrollToTop "#editor-view"
                , Dom.scrollTreeItemIntoView ("[data-km-editor-link=\"" ++ activeEditorUuid ++ "\"]")
                , Dom.focus ("[data-editor-uuid=\"" ++ activeEditorUuid ++ "\"] input")
                ]

        KMEditorRoute (EditorRoute _ KMEditorRoute.Preview) ->
            case model.editorContext of
                Success editorContext ->
                    Dom.scrollIntoView ("#question-" ++ EditorContext.getActiveQuestionUuid editorContext)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


fetchSubrouteDataFromAfter : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
fetchSubrouteDataFromAfter wrapMsg appState model =
    case ( model.editorContext, appState.route ) of
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

        GetKnowledgeModelEditorComplete result ->
            case result of
                Ok kmEditor ->
                    if KnowledgeModelEditorState.isEditable kmEditor.state then
                        let
                            ( newSeed, newModel, fetchCmd ) =
                                fetchSubrouteDataFromAfter wrapMsg
                                    appState
                                    { model
                                        | editorContext = Success (EditorContext.init appState (getSecrets model) kmEditor model.mbEditorUuid)
                                        , settingsModel = Settings.setKnowledgeModelEditorDetail appState kmEditor model.settingsModel
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
                        ( { model | editorContext = ApiError.toActionResult appState (gettext "Unable to get the knowledge model editor." appState.locale) error }
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
                            , editorContext = ActionResult.map (EditorContext.computeWarnings appState (List.map .name secrets)) model.editorContext
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
            withSeed ( model, Window.refresh () )

        KMEditorMsg kmEditorMsg ->
            case model.editorContext of
                Success editorContext ->
                    let
                        updateConfig =
                            { setFullscreenMsg = Wizard.Msgs.SetFullscreen
                            , wrapMsg = wrapMsg << KMEditorMsg
                            , eventMsg = \shouldDebounce mbFocusSelector mbFocusCaretPosition parentUuid mbEntityUuid createEvent -> wrapMsg <| EventMsg shouldDebounce mbFocusSelector mbFocusCaretPosition parentUuid mbEntityUuid createEvent
                            }

                        ( newEditorContext, newKmEditorModel, cmd ) =
                            KMEditor.update appState updateConfig kmEditorMsg ( editorContext, model.kmEditorModel )
                    in
                    withSeed ( { model | kmEditorModel = newKmEditorModel, editorContext = Success newEditorContext }, cmd )

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
            case model.editorContext of
                Success editorContext ->
                    let
                        ( newSeed, previewModel, cmd ) =
                            Preview.update previewMsg appState editorContext model.previewModel
                    in
                    ( newSeed, { model | previewModel = previewModel }, Cmd.map (wrapMsg << PreviewMsg) cmd )

                _ ->
                    withSeed ( model, Cmd.none )

        SettingsMsg settingsMsg ->
            let
                updateConfig =
                    { wrapMsg = wrapMsg << SettingsMsg
                    , cmdNavigate = cmdNavigate
                    , kmEditorUuid = model.uuid
                    }

                ( settingsModel, cmd ) =
                    Settings.update updateConfig appState settingsMsg model.settingsModel
            in
            withSeed ( { model | settingsModel = settingsModel }, cmd )

        PublishModalMsg publishModalMsg ->
            let
                publishModalUpdateConfig =
                    { wrapMsg = wrapMsg << PublishModalMsg
                    , kmEditorUuid = model.uuid
                    }

                ( publishModalModel, publishModalCmd ) =
                    PublishModal.update publishModalUpdateConfig appState publishModalMsg model.publishModalModel
            in
            withSeed ( { model | publishModalModel = publishModalModel }, publishModalCmd )

        EventMsg shouldDebounce mbFocusSelector mbFocusCaretPosition parentUuid mbEntityUuid eventContent ->
            let
                ( kmEventUuid, newSeed1 ) =
                    Uuid.stepString appState.seed

                ( wsEventUuid, newSeed2 ) =
                    Uuid.step newSeed1

                ( entityUuid, newSeed3 ) =
                    case mbEntityUuid of
                        Just eUuid ->
                            ( eUuid, newSeed2 )

                        Nothing ->
                            Uuid.stepString newSeed2

                event =
                    { uuid = kmEventUuid
                    , parentUuid = parentUuid
                    , entityUuid = entityUuid
                    , content = eventContent
                    , createdAt = appState.currentTime
                    }

                newEditorContext =
                    ActionResult.map (EditorContext.applyEvent appState (getSecrets model) True event) model.editorContext

                setUnloadMessageCmd =
                    Window.setUnloadMessage (gettext "Some changes are still saving." appState.locale)

                focusSelectorCmd =
                    case mbFocusSelector of
                        Just selector ->
                            case mbFocusCaretPosition of
                                Just caretPosition ->
                                    Dom.focusAndSetCaret selector caretPosition

                                Nothing ->
                                    Dom.focus selector

                        Nothing ->
                            Cmd.none
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
                        SetContentKnowledgeModelEditorAction.AddKnowledgeModelEditorWebSocketEvent
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
                    | editorContext = newEditorContext
                    , kmEditorModel = KMEditor.closeAllModals model.kmEditorModel
                    , eventsWebsocketDebounce = Dict.insert entityUuid debounce model.eventsWebsocketDebounce
                    , eventsLastEvent = Dict.insert entityUuid squashedEvent model.eventsLastEvent
                  }
                , Cmd.batch [ Cmd.map wrapMsg debounceCmd, setUnloadMessageCmd, focusSelectorCmd ]
                )

            else
                let
                    wsEvent =
                        SetContentKnowledgeModelEditorAction.AddKnowledgeModelEditorWebSocketEvent
                            { uuid = wsEventUuid
                            , event = event
                            }

                    newModel =
                        addSavingActionUuid wsEventUuid model

                    wsCmd =
                        wsEvent
                            |> ClientKnowledgeModelEditorAction.SetContent
                            |> ClientKnowledgeModelEditorAction.encode
                            |> WebSocket.send model.websocket

                    navigateCmd =
                        getNavigateCmd appState model.uuid model.editorContext newEditorContext
                in
                ( newSeed3
                , { newModel
                    | editorContext = newEditorContext
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
                                |> ClientKnowledgeModelEditorAction.SetContent
                                |> ClientKnowledgeModelEditorAction.encode
                                |> WebSocket.send model.websocket
                    in
                    Cmd.batch
                        [ wsCmd
                        , Task.dispatch (EventAddSavingUuid (SetContentKnowledgeModelEditorAction.getUuid event) entityUuid)
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
                    Uuid.step appState.seed

                event =
                    { uuid = eventUuid
                    , replies = model.previewModel.questionnaireModel.questionnaire.replies
                    }

                wsCmd =
                    event
                        |> ClientKnowledgeModelEditorAction.SetReplies
                        |> ClientKnowledgeModelEditorAction.encode
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
                        { newModel | editorContext = ActionResult.map (EditorContext.applyEvent appState (getSecrets model) False eventData.event) newModel.editorContext }

                    else
                        newModel

                clearUnloadMessageCmd =
                    if removed && List.isEmpty newModel2.savingActionUuids then
                        Window.clearUnloadMessage ()

                    else
                        Cmd.none

                navigateCmd =
                    getNavigateCmd appState model.uuid model.editorContext newModel2.editorContext
            in
            ( appState.seed
            , newModel2
            , Cmd.batch [ clearUnloadMessageCmd, navigateCmd ]
            )
    in
    case WebSocket.receive (WebSocketServerAction.decoder ServerKnowledgeModelEditorAction.decoder) websocketMsg model.websocket of
        WebSocket.Message serverAction ->
            case serverAction of
                WebSocketServerAction.Success message ->
                    case message of
                        ServerKnowledgeModelEditorAction.SetUserList users ->
                            ( appState.seed, { model | onlineUsers = users }, Cmd.none )

                        ServerKnowledgeModelEditorAction.SetContent setContentKnowledgeModelEditorAction ->
                            case setContentKnowledgeModelEditorAction of
                                SetContentKnowledgeModelEditorAction.AddKnowledgeModelEditorWebSocketEvent data ->
                                    updateModel data

                        ServerKnowledgeModelEditorAction.SetReplies event ->
                            let
                                ( newModel, _ ) =
                                    removeSavingActionUuid event.uuid model
                            in
                            ( appState.seed
                            , { newModel
                                | editorContext = ActionResult.map (EditorContext.setReplies event.replies) newModel.editorContext
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


getNavigateCmd : AppState -> Uuid -> ActionResult EditorContext -> ActionResult EditorContext -> Cmd msg
getNavigateCmd appState uuid oldEditorContext newEditorContext =
    let
        originalActiveEditor =
            ActionResult.unwrap "" .activeUuid oldEditorContext

        newActiveEditor =
            ActionResult.unwrap "" .activeUuid newEditorContext
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


getDebounceModel : String -> Model -> Debounce.Debounce SetContentKnowledgeModelEditorAction
getDebounceModel entityUuid model =
    Maybe.withDefault Debounce.init (Dict.get entityUuid model.eventsWebsocketDebounce)
