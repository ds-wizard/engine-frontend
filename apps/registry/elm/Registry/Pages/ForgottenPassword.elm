module Registry.Pages.ForgottenPassword exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

{-| A forgotten password: the e-mail to send the link to
(`Registry.Pages.ResetPassword` is where it leads).
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Form as Form
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Registry.Api as Api
import Registry.Components.Fields as Fields
import Registry.Data.AppState exposing (AppState)
import Ui.Alert as Alert
import Ui.Text as Text


type alias Model =
    { email : String
    , submitted : Bool
    , result : ActionResult ()
    }


init : Model
init =
    { email = "", submitted = False, result = Unset }


type Msg
    = SetEmail String
    | Submit
    | Completed (Result ApiError ())


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        Submit ->
            if ActionResult.isLoading model.result then
                ( model, Cmd.none )

            else if Maybe.isNothing (Fields.emailError appState model.email) then
                ( { model | submitted = True, result = Loading }
                , Api.postForgottenPassword appState (String.trim model.email) Completed
                )

            else
                ( { model | submitted = True }, Cmd.none )

        Completed (Ok _) ->
            ( { model | result = Success () }, Cmd.none )

        Completed (Err error) ->
            ( { model | result = ApiError.toActionResult appState (gettext "Forgotten password recovery failed." appState.locale) error }
            , Form.focusErrorSummary
            )


view : AppState -> Model -> { title : String, content : List (Html Msg) }
view appState model =
    { title = gettext "Forgotten Password" appState.locale
    , content =
        case model.result of
            Success _ ->
                [ Alert.toHtml (Alert.success (gettext "We've sent you a recover link. Follow the instructions in your email." appState.locale)) ]

            _ ->
                [ Text.body (gettext "Enter the email you use to log in, and we will send you a recover link." appState.locale)
                , Form.view
                    { onSubmit = Submit
                    , content =
                        [ Form.errorSummary model.result
                        , Fields.field
                            { id = "email"
                            , label = gettext "Email" appState.locale
                            , kind = Fields.Email
                            , value = model.email
                            , onInput = SetEmail
                            , error =
                                if model.submitted then
                                    Fields.emailError appState model.email

                                else
                                    Nothing
                            }
                        , Form.actions appState { submitLabel = gettext "Recover" appState.locale, result = model.result, onCancel = Nothing }
                        ]
                    }
                ]
    }
