module Common.Pages.Public.ForgottenPassword exposing
    ( Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Ask for a link to reset a forgotten password.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.UserEmailLinks as UserEmailLinksApi
import Common.Components.Form as FormView
import Common.Components.Form.Field as FormField
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.ForgottenPasswordForm as ForgottenPasswordForm exposing (ForgottenPasswordForm)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Public.Layout as PublicLayout
import Common.Ports.Dom as Dom
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Alert as Alert
import Ui.AppShell as AppShell


type alias Model =
    { form : Form FormError ForgottenPasswordForm
    , submitting : ActionResult String
    }


initialModel : Model
initialModel =
    { form = ForgottenPasswordForm.initEmpty
    , submitting = Unset
    }


type Msg
    = FormMsg Form.Msg
    | PostForgottenPasswordCompleted (Result ApiError ())


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form ) of
                ( Form.Submit, Just forgottenPasswordForm ) ->
                    ( { model | submitting = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        UserEmailLinksApi.postUserEmailLink appState (ForgottenPasswordForm.encode forgottenPasswordForm) PostForgottenPasswordCompleted
                    )

                _ ->
                    ( { model | form = Form.update ForgottenPasswordForm.validation formMsg model.form }
                    , Cmd.none
                    )

        PostForgottenPasswordCompleted result ->
            case result of
                Ok _ ->
                    -- The form is replaced by the message, so the button that sent it is gone.
                    ( { model | submitting = Success "" }, Dom.focus ("#" ++ AppShell.mainHeadingId) )

                Err error ->
                    ( { model
                        | submitting = ApiError.toActionResult appState (gettext "Forgotten password recovery failed." appState.locale) error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , FormView.focusErrorSummary
                    )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    case model.submitting of
        Success _ ->
            Sub.none

        _ ->
            FormView.submitShortcut appState { result = model.submitting, onSubmit = FormMsg Form.Submit }


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.submitting of
                Success _ ->
                    Alert.toHtml (Alert.success (gettext "Check your email. If you have an account, you should receive an email with instructions on how to reset your password." appState.locale))

                _ ->
                    FormView.view
                        { onSubmit = FormMsg Form.Submit
                        , content =
                            [ FormView.errorSummary model.submitting
                            , FormField.email appState model.form "email" (gettext "Email" appState.locale)
                                |> FormField.withRequired True
                                |> FormField.withAutocomplete "email"
                                |> FormField.withHint (gettext "Enter the email you use to log in and we will send you a recover link." appState.locale)
                                |> FormField.toHtml
                                |> Html.map FormMsg
                            , FormView.actions appState
                                { submitLabel = gettext "Recover" appState.locale
                                , result = model.submitting
                                , onCancel = Nothing
                                }
                            ]
                        }
    in
    PublicLayout.panel appState { title = gettext "Forgotten Password" appState.locale, content = [ content ], intro = Nothing, aside = Nothing }
