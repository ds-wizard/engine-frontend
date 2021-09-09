module Wizard exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Json.Decode exposing (Value)
import Shared.Utils exposing (dispatch)
import Url exposing (Url)
import Wizard.Common.AppState as AppState
import Wizard.Common.Time as Time
import Wizard.KnowledgeModels.Routes as KnowledgeModelsRoute
import Wizard.Models exposing (Model, initLocalModel, initialModel, userLoggedIn)
import Wizard.Msgs exposing (Msg)
import Wizard.Ports as Ports
import Wizard.Projects.Routes as PlansRoutes exposing (Route(..))
import Wizard.Public.Routes
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate, loginRoute, routeIfAllowed, toUrl)
import Wizard.Subscriptions exposing (subscriptions)
import Wizard.Update exposing (update)
import Wizard.View exposing (view)


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init flags location key =
    let
        originalRoute =
            Routing.parseLocation appState location

        route =
            routeIfAllowed appState originalRoute

        appState =
            AppState.init flags key

        appStateWithRoute =
            { appState | route = route }

        model =
            initLocalModel <| initialModel appStateWithRoute

        cmd =
            if appState.invalidSession then
                Ports.clearSessionAndReload ()

            else
                Cmd.batch
                    [ decideInitialRoute model location route originalRoute
                    , Time.getTime
                    , Time.getTimeZone
                    ]
    in
    ( model, cmd )


decideInitialRoute : Model -> Url -> Routes.Route -> Routes.Route -> Cmd Msg
decideInitialRoute model location route originalRoute =
    let
        dispatchUrlChange =
            dispatch (Wizard.Msgs.OnUrlChange location)
    in
    case route of
        Routes.PublicRoute subroute ->
            case ( userLoggedIn model, subroute ) of
                ( True, Wizard.Public.Routes.BookReferenceRoute _ ) ->
                    dispatchUrlChange

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

        _ ->
            if userLoggedIn model then
                dispatchUrlChange

            else
                cmdNavigate model.appState (loginRoute <| Just <| toUrl model.appState originalRoute)


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
