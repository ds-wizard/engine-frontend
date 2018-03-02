module Main exposing (main)

import Auth.Models exposing (Session, initialSession, parseJwt)
import Json.Decode as Decode exposing (Value)
import KnowledgeModels.Editor.Subscriptions
import Models exposing (..)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Routing exposing (Route(..), cmdNavigate, homeRoute, routeIfAllowed)
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
        Public _ ->
            if userLoggedIn model then
                cmdNavigate Welcome
            else
                Cmd.none

        _ ->
            if userLoggedIn model then
                fetchData model
            else
                cmdNavigate homeRoute


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.route of
        KnowledgeModelsEditor _ ->
            KnowledgeModels.Editor.Subscriptions.subscriptions model.knowledgeModelsEditorModel

        _ ->
            Sub.none


main : Program Value Model Msg
main =
    Navigation.programWithFlags Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
