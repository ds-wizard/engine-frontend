module Registry.Pages.ForgottenToken exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, div, form, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Registry.Common.AppState as AppState exposing (AppState)
import Registry.Common.FormExtra exposing (CustomFormError)
import Registry.Common.Requests as Requests
import Registry.Common.View.ActionButton as ActionButton
import Registry.Common.View.FormGroup as FormGroup
import Registry.Common.View.FormResult as FormResult
import Registry.Common.View.Page as Page
import Result exposing (Result)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lx)


l_ : String -> AppState -> String
l_ =
    l "Registry.Pages.ForgottenToken"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Registry.Pages.ForgottenToken"


init : Model
init =
    { form = initRecoveryForm
    , submitting = Unset
    }



-- MODEL


type alias Model =
    { form : Form CustomFormError RecoveryForm
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
            ( ActionResult.apply setSubmitting (ApiError.toActionResult "Could not recover token.") result model
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
        { image = "confirmation"
        , heading = l_ "success.heading" appState
        , msg = l_ "success.msg" appState
        }


formView : AppState -> Model -> Html Msg
formView appState model =
    div [ class "card card-form bg-light" ]
        [ div [ class "card-header" ] [ lx_ "formView.header" appState ]
        , div [ class "card-body" ]
            [ form [ onSubmit <| FormMsg Form.Submit ]
                [ FormResult.errorOnlyView model.submitting
                , Html.map FormMsg <| FormGroup.input appState model.form "email" <| l_ "formView.email.label" appState
                , p [ class "text-muted" ]
                    [ lx_ "formView.email.help" appState ]
                , ActionButton.submit ( l_ "formView.submit" appState, model.submitting )
                ]
            ]
        ]
