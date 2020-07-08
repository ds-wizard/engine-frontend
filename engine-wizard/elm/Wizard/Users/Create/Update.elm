module Wizard.Users.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Random exposing (Seed, step)
import Shared.Api.Users as UsersApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (tuplePrepend)
import Uuid
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Users.Common.UserCreateForm as UserCreateForm
import Wizard.Users.Create.Models exposing (..)
import Wizard.Users.Create.Msgs exposing (Msg(..))
import Wizard.Users.Routes exposing (Route(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState.seed appState model

        PostUserCompleted result ->
            postUserCompleted appState model result |> tuplePrepend appState.seed


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> Seed -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg seed appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just userCreateForm ) ->
            let
                ( newUuid, newSeed ) =
                    step Uuid.uuidGenerator seed

                body =
                    UserCreateForm.encode (Uuid.toString newUuid) userCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.postUser body appState PostUserCompleted
            in
            ( newSeed, { model | savingUser = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update UserCreateForm.validation formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


postUserCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
postUserCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState <| Routes.UsersRoute IndexRoute )

        Err error ->
            ( { model | savingUser = ApiError.toActionResult (lg "apiError.users.postError" appState) error }
            , getResultCmd result
            )
