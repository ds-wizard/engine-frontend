module Users.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form exposing (Form)
import Msgs
import Random exposing (Seed, step)
import Routing exposing (cmdNavigate)
import Users.Create.Models exposing (..)
import Users.Create.Msgs exposing (Msg(..))
import Users.Routing exposing (Route(..))
import Utils exposing (tuplePrepend)
import Uuid


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState.seed appState model

        PostUserCompleted result ->
            postUserCompleted appState model result |> tuplePrepend appState.seed


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Seed -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg seed appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just userCreateForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                body =
                    encodeUserCreateForm (Uuid.toString newUuid) userCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.postUser body appState PostUserCompleted
            in
            ( newSeed, { model | savingUser = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update userCreateFormValidation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


postUserCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
postUserCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState.key <| Routing.Users Index )

        Err error ->
            ( { model | savingUser = getServerError error "User could not be created." }
            , getResultCmd result
            )
