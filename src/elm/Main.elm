module Main exposing (..)

import Json.Decode as Decode exposing (Value)
import Models exposing (Model, initialModel, userLoggedIn)
import Msgs exposing (Msg)
import Navigation exposing (Location)
import Routing exposing (Route(..), cmdNavigate)
import Update exposing (update)
import View exposing (view)


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    let
        currentRoute =
            Routing.parseLocation location

        token =
            case decodeTokenFromJson val of
                Just token ->
                    token

                Nothing ->
                    ""

        model =
            initialModel currentRoute token
    in
    ( model, decideInitialRoute model currentRoute )


decideInitialRoute : Model -> Route -> Cmd msg
decideInitialRoute model route =
    case route of
        Login ->
            if userLoggedIn model then
                cmdNavigate Index
            else
                Cmd.none

        _ ->
            if userLoggedIn model then
                Cmd.none
            else
                cmdNavigate Login


decodeTokenFromJson : Value -> Maybe String
decodeTokenFromJson json =
    json
        |> Decode.decodeValue Decode.string
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
