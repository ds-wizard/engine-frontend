module Users.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Form exposing (Form)
import Jwt
import Models exposing (State)
import Msgs
import Random exposing (Seed, step)
import Requests exposing (getResultCmd)
import Routing exposing (cmdNavigate)
import Users.Create.Models exposing (..)
import Users.Create.Msgs exposing (Msg(..))
import Users.Requests exposing (postUser)
import Users.Routing exposing (Route(..))
import Utils exposing (tuplePrepend)
import Uuid


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg state.seed state.session model

        PostUserCompleted result ->
            postUserCompleted state model result |> tuplePrepend state.seed


handleForm : Form.Msg -> (Msg -> Msgs.Msg) -> Seed -> Session -> Model -> ( Seed, Model, Cmd Msgs.Msg )
handleForm formMsg wrapMsg seed session model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just userCreateForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                cmd =
                    Uuid.toString newUuid
                        |> postUserCmd session userCreateForm
                        |> Cmd.map wrapMsg
            in
            ( newSeed, { model | savingUser = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update userCreateFormValidation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


postUserCmd : Session -> UserCreateForm -> String -> Cmd Msg
postUserCmd session form uuid =
    form
        |> encodeUserCreateForm uuid
        |> postUser session
        |> Jwt.send PostUserCompleted


postUserCompleted : State -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
postUserCompleted state model result =
    case result of
        Ok user ->
            ( model, cmdNavigate state.key <| Routing.Users Index )

        Err error ->
            ( { model | savingUser = getServerErrorJwt error "User could not be created." }
            , getResultCmd result
            )
