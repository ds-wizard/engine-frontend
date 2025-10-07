module Wizard.Pages.Users.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Ports.Dom as Dom
import Common.Ports.Window as Window
import Common.Utils.Form as Form
import Common.Utils.RequestHelpers as RequestHelpers
import Form
import Gettext exposing (gettext)
import Random exposing (Seed, step)
import Tuple.Extensions as Tuple
import Uuid
import Wizard.Api.Users as UsersApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Users.Common.UserCreateForm as UserCreateForm
import Wizard.Pages.Users.Create.Models exposing (Model)
import Wizard.Pages.Users.Create.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Cancel ->
            ( appState.seed, model, Window.historyBack (Routing.toUrl Routes.usersIndex) )

        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState.seed appState model

        PostUserCompleted result ->
            postUserCompleted appState model result |> Tuple.prepend appState.seed


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
                        UsersApi.postUser appState body PostUserCompleted
            in
            ( newSeed, { model | savingUser = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update (UserCreateForm.validation appState) formMsg model.form }
            in
            ( seed, newModel, Cmd.none )


postUserCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
postUserCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.usersIndex )

        Err error ->
            ( { model
                | savingUser = ApiError.toActionResult appState (gettext "User could not be created." appState.locale) error
                , form = Form.setFormErrors appState error model.form
              }
            , Cmd.batch
                [ RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                , Dom.scrollToTop "html"
                ]
            )
