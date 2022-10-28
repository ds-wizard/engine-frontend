module Registry.Pages.ForgottenToken exposing
    ( Model
    , Msg
    , RecoveryForm
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Html exposing (Html, div, form, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Registry.Common.AppState exposing (AppState)
import Registry.Common.Requests as Requests
import Registry.Common.View.ActionButton as ActionButton
import Registry.Common.View.FormGroup as FormGroup
import Registry.Common.View.FormResult as FormResult
import Registry.Common.View.Page as Page
import Result exposing (Result)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Undraw as Undraw


init : Model
init =
    { form = initRecoveryForm
    , submitting = Unset
    }



-- MODEL


type alias Model =
    { form : Form FormError RecoveryForm
    , submitting : ActionResult ()
    }


type alias RecoveryForm =
    { email : String }


setSubmitting : ActionResult () -> Model -> Model
setSubmitting submitting model =
    { model | submitting = submitting }


recoveryFormValidation : Validation e RecoveryForm
recoveryFormValidation =
    Validate.map RecoveryForm
        (Validate.field "email" Validate.email)


initRecoveryForm : Form e RecoveryForm
initRecoveryForm =
    Form.initial [] recoveryFormValidation



-- UPDATE


type Msg
    = FormMsg Form.Msg
    | PostForgottenTokenActionKeyCompleted (Result ApiError ())


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        PostForgottenTokenActionKeyCompleted result ->
            ( ActionResult.apply setSubmitting (ApiError.toActionResult appState "Could not recover token.") result model
            , Cmd.none
            )


handleFormMsg : Form.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg formMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just recoveryForm ) ->
            ( { model | submitting = Loading }
            , Requests.postForgottenTokenActionKey recoveryForm appState PostForgottenTokenActionKeyCompleted
            )

        _ ->
            ( { model | form = Form.update recoveryFormValidation formMsg model.form }
            , Cmd.none
            )



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    if ActionResult.isSuccess model.submitting then
        successView appState

    else
        formView appState model


successView : AppState -> Html Msg
successView appState =
    Page.illustratedMessage
        { image = Undraw.confirmation
        , heading = gettext "Token recovery successful!" appState.locale
        , msg = gettext "Check your email address for the recovery link." appState.locale
        }


formView : AppState -> Model -> Html Msg
formView appState model =
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ text (gettext "Forgotten Token" appState.locale) ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.submitting
                , Html.map FormMsg <| FormGroup.input appState model.form "email" <| gettext "Email" appState.locale
                , p [ class "text-muted" ]
                    [ text (gettext "Enter the email you used to register your organization." appState.locale) ]
                , ActionButton.submit ( gettext "Submit" appState.locale, model.submitting )
                ]
            ]
        ]
