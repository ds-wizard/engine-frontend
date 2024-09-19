module Wizard exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Json.Decode exposing (Value)
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Utils as Taks exposing (dispatch)
import Shared.Utils.Theme as Theme
import Url exposing (Url)
import Wizard.Common.AppState as AppState
import Wizard.Common.Components.AIAssistant as AIAssistant
import Wizard.Common.Time as Time
import Wizard.KnowledgeModels.Routes as KnowledgeModelsRoute
import Wizard.Models exposing (Model, initLocalModel, initialModel, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Ports as Ports
import Wizard.Projects.Routes as PlansRoutes
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
            initLocalModel <| initialModel appState

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
                        if Admin.isEnabled appState.config.admin then
                            Taks.dispatch (Wizard.Msgs.AIAssistantMsg AIAssistant.init)

                        else
                            Cmd.none
                in
                Cmd.batch
                    [ decideInitialRoute model location route originalRoute
                    , setThemeCmd
                    , aiAssistantCmd
                    , Time.getTime
                    , Time.getTimeZone
                    ]
    in
    ( model, Cmd.batch [ cmd, appStateCmd ] )


decideInitialRoute : Model -> Url -> Routes.Route -> Routes.Route -> Cmd Msg
decideInitialRoute model location route originalRoute =
    let
        dispatchUrlChange =
            dispatch (Wizard.Msgs.OnUrlChange location)
    in
    case route of
        Routes.PublicRoute subroute ->
            case ( userLoggedIn model, subroute ) of
                ( True, _ ) ->
                    cmdNavigate model.appState Routes.DashboardRoute

                _ ->
                    dispatchUrlChange

        Routes.ProjectsRoute (PlansRoutes.DetailRoute _ _) ->
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
                cmdNavigate model.appState (Routes.publicLogin <| Just <| toUrl model.appState originalRoute)


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
