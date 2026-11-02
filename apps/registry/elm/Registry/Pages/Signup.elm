module Registry.Pages.Signup exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| Making an account: a name, an e-mail and a password. The account is active
once the link the registry e-mails is followed (`Registry.Pages.Confirmation`).
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Form as Form
import Gettext exposing (gettext)
import Html exposing (Html)
import Registry.Api as Api
import Registry.Components.Fields as Fields
import Registry.Data.AppState exposing (AppState)
import Ui.Alert as Alert


type alias Model =
    { firstName : String
    , lastName : String
    , email : String
    , password : String
    , repeatedPassword : String
    , submitted : Bool
    , result : ActionResult ()
    }


init : Model
init =
    { firstName = "", lastName = "", email = "", password = "", repeatedPassword = "", submitted = False, result = Unset }


type Msg
    = SetFirstName String
    | SetLastName String
    | SetEmail String
    | SetPassword String
    | SetRepeatedPassword String
    | Submit
    | Completed (Result ApiError ())


type alias Errors =
    { firstName : Maybe String
    , lastName : Maybe String
    , email : Maybe String
    , password : Maybe String
    , repeatedPassword : Maybe String
    }


errors : AppState -> Model -> Errors
errors appState model =
    { firstName = Fields.requiredError appState model.firstName
    , lastName = Fields.requiredError appState model.lastName
    , email = Fields.emailError appState model.email
    , password = Fields.passwordError appState model.password
    , repeatedPassword = Fields.repeatError appState model.password model.repeatedPassword
    }


isValid : Errors -> Bool
isValid found =
    List.all ((==) Nothing) [ found.firstName, found.lastName, found.email, found.password, found.repeatedPassword ]


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        SetFirstName value ->
            ( { model | firstName = value }, Cmd.none )

        SetLastName value ->
            ( { model | lastName = value }, Cmd.none )

        SetEmail value ->
            ( { model | email = value }, Cmd.none )

        SetPassword value ->
            ( { model | password = value }, Cmd.none )

        SetRepeatedPassword value ->
            ( { model | repeatedPassword = value }, Cmd.none )

        Submit ->
            if ActionResult.isLoading model.result then
                ( model, Cmd.none )

            else if isValid (errors appState model) then
                ( { model | submitted = True, result = Loading }
                , Api.postUser appState
                    { email = String.trim model.email
                    , firstName = String.trim model.firstName
                    , lastName = String.trim model.lastName
                    , password = model.password
                    }
                    Completed
                )

            else
                ( { model | submitted = True }, Cmd.none )

        Completed result ->
            case result of
                Ok _ ->
                    ( { model | result = Success () }, Cmd.none )

                Err error ->
                    ( { model | result = ApiError.toActionResult appState (gettext "Sign up process failed." appState.locale) error }
                    , Form.focusErrorSummary
                    )


view : AppState -> Model -> { title : String, content : List (Html Msg) }
view appState model =
    { title = gettext "Sign up" appState.locale
    , content =
        case model.result of
            Success _ ->
                [ Alert.toHtml (Alert.success (gettext "Sign up was successful. Check your email for the activation link." appState.locale)) ]

            _ ->
                let
                    found =
                        if model.submitted then
                            errors appState model

                        else
                            Errors Nothing Nothing Nothing Nothing Nothing
                in
                [ Form.view
                    { onSubmit = Submit
                    , content =
                        [ Form.errorSummary model.result
                        , Fields.field { id = "firstName", label = gettext "First name" appState.locale, kind = Fields.Text "given-name", value = model.firstName, onInput = SetFirstName, error = found.firstName }
                        , Fields.field { id = "lastName", label = gettext "Last name" appState.locale, kind = Fields.Text "family-name", value = model.lastName, onInput = SetLastName, error = found.lastName }
                        , Fields.field { id = "email", label = gettext "Email" appState.locale, kind = Fields.Email, value = model.email, onInput = SetEmail, error = found.email }
                        , Fields.field { id = "password", label = gettext "Password" appState.locale, kind = Fields.NewPassword, value = model.password, onInput = SetPassword, error = found.password }
                        , Fields.field { id = "passwordConfirmation", label = gettext "Password confirmation" appState.locale, kind = Fields.NewPassword, value = model.repeatedPassword, onInput = SetRepeatedPassword, error = found.repeatedPassword }
                        , Form.actions appState { submitLabel = gettext "Sign up" appState.locale, result = model.result, onCancel = Nothing }
                        ]
                    }
                ]
    }
