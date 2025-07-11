module Wizard.Users.Edit.Components.Password exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Shared.Common.UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Ports as Ports
import Wizard.Users.Common.UserPasswordForm as UserPasswordForm exposing (UserPasswordForm)


type alias Model =
    { uuidOrCurrent : UuidOrCurrent
    , passwordForm : Form FormError UserPasswordForm
    , savingPassword : ActionResult String
    }


initialModel : AppState -> UuidOrCurrent -> Model
initialModel appState uuidOrCurrent =
    { uuidOrCurrent = uuidOrCurrent
    , passwordForm = UserPasswordForm.init appState
    , savingPassword = ActionResult.Unset
    }


type Msg
    = PasswordFormMsg Form.Msg
    | PutUserPasswordCompleted (Result ApiError ())


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        PasswordFormMsg formMsg ->
            handlePasswordForm cfg appState formMsg model

        PutUserPasswordCompleted result ->
            putUserPasswordCompleted cfg appState model result


handlePasswordForm : UpdateConfig msg -> AppState -> Form.Msg -> Model -> ( Model, Cmd msg )
handlePasswordForm cfg appState formMsg model =
    case ( formMsg, Form.getOutput model.passwordForm ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                body =
                    UserPasswordForm.encode passwordForm

                cmd =
                    Cmd.map cfg.wrapMsg <|
                        UsersApi.putUserPassword appState model.uuidOrCurrent body PutUserPasswordCompleted
            in
            ( { model | savingPassword = ActionResult.Loading }, cmd )

        _ ->
            let
                passwordForm =
                    Form.update (UserPasswordForm.validation appState) formMsg model.passwordForm
            in
            ( { model | passwordForm = passwordForm }, Cmd.none )


putUserPasswordCompleted : UpdateConfig msg -> AppState -> Model -> Result ApiError () -> ( Model, Cmd msg )
putUserPasswordCompleted cfg appState model result =
    let
        passwordResult =
            case result of
                Ok _ ->
                    ActionResult.Success <| gettext "Password was successfully changed." appState.locale

                Err error ->
                    ApiError.toActionResult appState (gettext "Password could not be changed." appState.locale) error

        cmd =
            RequestHelpers.getResultCmd cfg.logoutMsg result
    in
    ( { model | savingPassword = passwordResult }
    , Cmd.batch [ cmd, Ports.scrollToTop ".Users__Edit__content" ]
    )


view : AppState -> Model -> Html Msg
view appState model =
    Html.form [ onSubmit (PasswordFormMsg Form.Submit), detailClass "" ]
        [ Page.header (gettext "Password" appState.locale) []
        , FormResult.view model.savingPassword
        , passwordFormView appState model.passwordForm |> Html.map PasswordFormMsg
        , div [ class "mt-5" ]
            [ ActionButton.submit (ActionButton.SubmitConfig (gettext "Save" appState.locale) model.savingPassword) ]
        ]


passwordFormView : AppState -> Form FormError UserPasswordForm -> Html Form.Msg
passwordFormView appState form =
    div []
        [ FormGroup.passwordWithStrength appState form "password" <| gettext "New password" appState.locale
        , FormGroup.password appState form "passwordConfirmation" <| gettext "New password again" appState.locale
        ]
