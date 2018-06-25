module Main exposing (main)

import Auth.Models exposing (Session, initialSession, parseJwt)
import Json.Decode as Decode exposing (Value)
import Models exposing (..)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Public.Routing
import Routing exposing (Route(..), cmdNavigate, homeRoute, routeIfAllowed)
import Subscriptions exposing (subscriptions)
import Update exposing (fetchData, update)
import View exposing (view)


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        ( session, jwt, seed ) =
            case decodeFlagsFromJson val of
                Just flags ->
                    case flags.session of
                        Just session ->
                            ( session, parseJwt session.token, flags.seed )

                        Nothing ->
                            ( initialSession, Nothing, flags.seed )

                Nothing ->
                    ( initialSession, Nothing, 0 )

        route =
            location
                |> Routing.parseLocation
                |> routeIfAllowed jwt

        model =
            initialModel route seed session jwt
    in
    ( initLocalModel model, decideInitialRoute model route )


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
                    cmdNavigate Welcome

                _ ->
                    fetchData model

        _ ->
            if userLoggedIn model then
                fetchData model
            else
                cmdNavigate homeRoute


main : Program Value Model Msg
main =
    Navigation.programWithFlags Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
