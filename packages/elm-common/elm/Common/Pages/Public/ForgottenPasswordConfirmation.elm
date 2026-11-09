module Common.Pages.Public.ForgottenPasswordConfirmation exposing
    ( Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Set a new password from the link in a password-reset email.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Users as UsersApi
import Common.Components.Form as FormView
import Common.Components.Form.Field as FormField
import Common.Components.PasswordStrength as PasswordStrength
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.PasswordForm as PasswordForm exposing (PasswordForm)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Public.Layout as PublicLayout
import Common.Ports.Dom as Dom
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, p)
import String.Format as String
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Link as Link
import Ui.Text as Text


type alias Model =
    { form : Form FormError PasswordForm
    , submitting : ActionResult String
    , userId : String
    , hash : String
    , passwordStrength : PasswordStrength.Model
    }


initialModel : AppState -> String -> String -> Model
initialModel appState userId hash =
    { form = PasswordForm.initEmpty appState
    , submitting = Unset
    , userId = userId
    , hash = hash
    , passwordStrength = PasswordStrength.init appState
    }


type Msg
    = FormMsg Form.Msg
    | PutPasswordCompleted (Result ApiError ())
    | PasswordStrengthMsg PasswordStrength.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, PasswordStrength.isChecking model.passwordStrength ) of
                ( Form.Submit, Just passwordForm, False ) ->
                    ( { model | submitting = Loading }
                    , Cmd.map cfg.wrapMsg <|
                        UsersApi.putUserPasswordPublic appState model.userId model.hash (PasswordForm.encode passwordForm) PutPasswordCompleted
                    )

                _ ->
                    let
                        updated =
                            PasswordStrength.updateForm appState (passwordField appState) formMsg model.form model.passwordStrength
                    in
                    ( { model | form = updated.form, passwordStrength = updated.passwordStrength }
                    , PasswordStrength.toCmd (cfg.wrapMsg << PasswordStrengthMsg) (cfg.wrapMsg (FormMsg Form.Submit)) updated
                    )

        PasswordStrengthMsg passwordStrengthMsg ->
            let
                updated =
                    PasswordStrength.update appState (passwordField appState) passwordStrengthMsg model.form model.passwordStrength
            in
            ( { model | form = updated.form, passwordStrength = updated.passwordStrength }
            , PasswordStrength.toCmd (cfg.wrapMsg << PasswordStrengthMsg) (cfg.wrapMsg (FormMsg Form.Submit)) updated
            )

        PutPasswordCompleted result ->
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


passwordField : AppState -> PasswordStrength.PasswordField PasswordForm
passwordField appState =
    { path = "password", validation = PasswordForm.validation appState }


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
                    Alert.success ""
                        |> Alert.toHtmlWithContent
                            [ p []
                                (String.formatHtml
                                    (gettext "Your password has been changed. You can now %s." appState.locale)
                                    [ Link.link { label = gettext "log in" appState.locale, url = Routes.toUrl appState (Routes.publicLogin Nothing) }
                                        |> Link.toHtml
                                    ]
                                )
                            ]

                _ ->
                    FormView.view
                        { onSubmit = FormMsg Form.Submit
                        , content =
                            [ FormView.errorSummary model.submitting
                            , Text.body (gettext "Enter a new password you want to use to log in." appState.locale)
                            , Html.map FormMsg <|
                                (FormField.passwordWithStrength appState model.passwordStrength model.form "password" (gettext "New password" appState.locale)
                                    |> FormField.withRequired True
                                    |> FormField.withAutocomplete "new-password"
                                    |> FormField.toHtml
                                )
                            , Html.map FormMsg <|
                                (FormField.password appState model.form "passwordConfirmation" (gettext "New password again" appState.locale)
                                    |> FormField.withRequired True
                                    |> FormField.withAutocomplete "new-password"
                                    |> FormField.toHtml
                                )
                            , FormView.actions appState
                                { submitLabel = gettext "Save" appState.locale
                                , result = model.submitting
                                , onCancel = Nothing
                                }
                            ]
                        }
    in
    PublicLayout.panel appState { title = gettext "Password Recovery" appState.locale, content = [ content ], intro = Nothing, aside = Nothing }
