module Main exposing (main)

import Auth.Models exposing (Session, initialSession, parseJwt)
import Browser
import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Value)
import Models exposing (..)
import Msgs exposing (Msg)
import Public.Routing
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
                |> Maybe.withDefault
                    { session = Nothing
                    , seed = 0
                    , apiUrl = ""
                    , appTitle = "Data Stewardship Wizard"
                    , appTitleShort = "DS Wizard"
                    , welcomeWarning = Nothing
                    , welcomeInfo = Nothing
                    }

        session =
            Maybe.withDefault initialSession flags.session

        jwt =
            Maybe.andThen (.token >> parseJwt) flags.session

        route =
            location
                |> Routing.parseLocation
                |> routeIfAllowed jwt

        model =
            initialModel route flags session jwt key
                |> initLocalModel
    in
    ( model, decideInitialRoute model route )


decodeFlagsFromJson : Value -> Maybe Flags
decodeFlagsFromJson =
    Decode.decodeValue flagsDecoder >> Result.toMaybe


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
                    cmdNavigate model.appState.key Welcome

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
