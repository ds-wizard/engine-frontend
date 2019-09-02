module Main exposing (main)

import Auth.Models exposing (Session, initialSession, parseJwt)
import Browser
import Browser.Navigation exposing (Key)
import Common.Time as Time
import Json.Decode as Decode exposing (Value)
import Models exposing (..)
import Msgs exposing (Msg)
import Public.Routes
import Random
import Routes
import Routing exposing (cmdNavigate, homeRoute, routeIfAllowed)
import Subscriptions exposing (subscriptions)
import Time
import Update exposing (fetchData, update)
import Url exposing (Url)
import View exposing (view)


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init val location key =
    let
        flags =
            decodeFlagsFromJson val

        session =
            Maybe.withDefault initialSession flags.session

        jwt =
            Maybe.andThen (.token >> parseJwt) flags.session

        route =
            location
                |> Routing.parseLocation appState
                |> routeIfAllowed jwt

        appState =
            { route = Routes.NotFoundRoute
            , seed = Random.initialSeed flags.seed
            , session = session
            , jwt = jwt
            , key = key
            , apiUrl = flags.apiUrl
            , config = flags.config
            , provisioning = flags.provisioning
            , valid = flags.success
            , currentTime = Time.millisToPosix 0
            }

        appStateWithRoute =
            { appState | route = route }

        model =
            initLocalModel <| initialModel appStateWithRoute
    in
    ( model
    , Cmd.batch
        [ decideInitialRoute model route
        , Time.getTime
        ]
    )


decodeFlagsFromJson : Value -> Flags
decodeFlagsFromJson =
    Decode.decodeValue flagsDecoder >> Result.withDefault defaultFlags


decideInitialRoute : Model -> Routes.Route -> Cmd Msg
decideInitialRoute model route =
    case route of
        Routes.PublicRoute subroute ->
            case ( userLoggedIn model, subroute ) of
                ( True, Public.Routes.BookReferenceRoute _ ) ->
                    fetchData model

                ( True, Public.Routes.QuestionnaireRoute ) ->
                    fetchData model

                ( True, _ ) ->
                    cmdNavigate model.appState Routes.DashboardRoute

                _ ->
                    fetchData model

        _ ->
            if userLoggedIn model then
                fetchData model

            else
                cmdNavigate model.appState homeRoute


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = Msgs.OnUrlChange
        , onUrlRequest = Msgs.OnUrlRequest
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
