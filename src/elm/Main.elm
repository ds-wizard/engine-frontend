module Main exposing (..)

import Auth.Models exposing (Session, decodeSession, initialSession, parseJwt)
import Json.Decode as Decode exposing (Value)
import Models exposing (..)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Routing exposing (Route(..), cmdNavigate, routeIfAllowed)
import Update exposing (fetchData, update)
import View exposing (view)


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        ( session, jwt ) =
            case decodeSessionFromJson val of
                Just session ->
                    ( session, parseJwt session.token )

                Nothing ->
                    ( initialSession, Nothing )

        route =
            location
                |> Routing.parseLocation
                |> routeIfAllowed jwt

        model =
            initialModel route session jwt
    in
    ( model, decideInitialRoute model route )


decideInitialRoute : Model -> Route -> Cmd Msg
decideInitialRoute model route =
    case route of
        Login ->
            if userLoggedIn model then
                cmdNavigate Index
            else
                Cmd.none

        _ ->
            if userLoggedIn model then
                fetchData model
            else
                cmdNavigate Login


decodeSessionFromJson : Value -> Maybe Session
decodeSessionFromJson json =
    json
        |> Decode.decodeValue decodeSession
        |> Result.toMaybe


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Value Model Msg
main =
    Navigation.programWithFlags Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
