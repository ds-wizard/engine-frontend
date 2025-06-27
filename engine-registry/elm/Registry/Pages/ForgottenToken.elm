module Registry.Pages.ForgottenToken exposing (Model, Msg, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Registry.Api.ActionKeys as ActionKeysApi
import Registry.Components.ActionButton as ActionButton
import Registry.Components.FormGroup as FormGroup
import Registry.Components.FormResult as FormResult
import Registry.Components.FormWrapper as FormWrapper
import Registry.Components.Page as Page
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Forms.ForgottenTokenForm as ForgottenTokenForm exposing (ForgottenTokenForm)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Undraw as Undraw


type alias Model =
    { form : Form FormError ForgottenTokenForm
    , submitting : ActionResult ()
    }


initialModel : Model
initialModel =
    { form = ForgottenTokenForm.init
    , submitting = ActionResult.Unset
    }


setSubmitting : ActionResult () -> Model -> Model
setSubmitting submitting model =
    { model | submitting = submitting }


type Msg
    = FormMsg Form.Msg
    | PostForgottenTokenActionKeyCompleted (Result ApiError ())


update : AppState -> Msg -> Model -> ( Model, Cmd Msg )
update appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just recoveryForm ) ->
                    ( { model | submitting = ActionResult.Loading }
                    , ActionKeysApi.postForgottenTokenActionKey appState recoveryForm PostForgottenTokenActionKeyCompleted
                    )

                _ ->
                    ( { model | form = Form.update ForgottenTokenForm.validation formMsg model.form }
                    , Cmd.none
                    )

        PostForgottenTokenActionKeyCompleted result ->
            ( ActionResult.apply setSubmitting (ApiError.toActionResult appState "Could not recover token.") result model
            , Cmd.none
            )


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
        , msg = gettext "Check your email for the recovery link." appState.locale
        }


formView : AppState -> Model -> Html Msg
formView appState model =
    FormWrapper.view
        { title = gettext "Forgotten Token" appState.locale
        , submitMsg = FormMsg Form.Submit
        , content =
            [ FormResult.errorOnlyView model.submitting
            , Html.map FormMsg <| FormGroup.input appState model.form "email" <| gettext "Email" appState.locale
            , p [ class "text-muted" ]
                [ text (gettext "Enter the email you used to register your organization." appState.locale) ]
            , ActionButton.view
                { label = gettext "Submit" appState.locale
                , actionResult = model.submitting
                }
            ]
        }
