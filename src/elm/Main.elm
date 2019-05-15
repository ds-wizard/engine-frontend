module Main exposing (main)

import Auth.Models exposing (Session, initialSession, parseJwt)
import Browser
import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Value)
import Models exposing (..)
import Msgs exposing (Msg)
import Public.Routing
import Random
import Routing exposing (Route(..), cmdNavigate, homeRoute, routeIfAllowed)
import Subscriptions exposing (subscriptions)
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
                |> Routing.parseLocation flags.config
                |> routeIfAllowed jwt

        appState =
            { route = route
            , seed = Random.initialSeed flags.seed
            , session = session
            , jwt = jwt
            , key = key
            , apiUrl = flags.apiUrl
            , config = flags.config
            , valid = flags.success
            }

        model =
            initialModel appState session jwt key
                |> initLocalModel
    in
    ( model, decideInitialRoute model route )


decodeFlagsFromJson : Value -> Flags
decodeFlagsFromJson =
    Decode.decodeValue flagsDecoder >> Result.withDefault defaultFlags


decideInitialRoute : Model -> Route -> Cmd Msg
decideInitialRoute model route =
    case route of
        Public subroute ->
            case ( userLoggedIn model, subroute ) of
                ( True, Public.Routing.BookReference _ ) ->
                    fetchData model

                ( True, Public.Routing.Questionnaire ) ->
                    fetchData model

                ( True, _ ) ->
                    cmdNavigate model.appState.key Dashboard

                _ ->
                    fetchData model

        _ ->
            if userLoggedIn model then
                fetchData model

            else
                cmdNavigate model.appState.key homeRoute


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
