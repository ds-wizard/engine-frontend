module Wizard exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Json.Decode exposing (Value)
import Shared.Components.AIAssistant as AIAssistant
import Shared.Utils.Theme as Theme
import Shared.Utils.TimeUtils as TimeUtils
import Task.Extra as Task
import Url exposing (Url)
import Wizard.Data.AppState as AppState
import Wizard.Data.Session as Session
import Wizard.Models exposing (Model, initLocalModel, initialModel, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Pages.KnowledgeModels.Routes as KnowledgeModelsRoute
import Wizard.Pages.Projects.Routes as ProjectsRoutes
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate, routeIfAllowed, toUrl)
import Wizard.Subscriptions exposing (subscriptions)
import Wizard.Update exposing (update)
import Wizard.View exposing (view)


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags location key =
    let
        ( appState, appStateCmd ) =
            AppState.init flags key

        model =
            initLocalModel appState <| initialModel appState

        cmd =
            if appState.invalidSession then
                Ports.clearSessionAndReload ()

            else
                let
                    originalRoute =
                        Routing.parseLocation appState location

                    route =
                        routeIfAllowed appState originalRoute

                    setThemeCmd =
                        case appState.theme of
                            Just theme ->
                                Theme.setTheme theme

                            Nothing ->
                                Cmd.none

                    aiAssistantCmd =
                        if appState.config.features.aiAssistantEnabled && Session.exists appState.session then
                            Task.dispatch (Wizard.Msgs.AIAssistantMsg AIAssistant.init)

                        else
                            Cmd.none
                in
                Cmd.batch
                    [ decideInitialRoute model location route originalRoute
                    , setThemeCmd
                    , aiAssistantCmd
                    , TimeUtils.getTime Wizard.Msgs.OnTime
                    , TimeUtils.getTimeZone Wizard.Msgs.OnTimeZone
                    ]
    in
    ( model, Cmd.batch [ cmd, appStateCmd ] )


decideInitialRoute : Model -> Url -> Routes.Route -> Routes.Route -> Cmd Msg
decideInitialRoute model location route originalRoute =
    let
        dispatchUrlChange =
            Task.dispatch (Wizard.Msgs.OnUrlChange location)
    in
    case route of
        Routes.PublicRoute subroute ->
            case ( userLoggedIn model, subroute ) of
                ( True, _ ) ->
                    cmdNavigate model.appState Routes.DashboardRoute

                _ ->
                    dispatchUrlChange

        Routes.ProjectsRoute (ProjectsRoutes.DetailRoute _ _) ->
            dispatchUrlChange

        Routes.ProjectsRoute (ProjectsRoutes.FileDownloadRoute _ _) ->
            dispatchUrlChange

        Routes.KnowledgeModelsRoute (KnowledgeModelsRoute.DetailRoute _) ->
            dispatchUrlChange

        Routes.KnowledgeModelsRoute (KnowledgeModelsRoute.PreviewRoute _ _) ->
            dispatchUrlChange

        Routes.KnowledgeModelsRoute (KnowledgeModelsRoute.ResourcePageRoute _ _) ->
            dispatchUrlChange

        _ ->
            if userLoggedIn model then
                dispatchUrlChange

            else
                cmdNavigate model.appState (Routes.publicLogin <| Just <| toUrl originalRoute)


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = Wizard.Msgs.OnUrlChange
        , onUrlRequest = Wizard.Msgs.OnUrlRequest
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
