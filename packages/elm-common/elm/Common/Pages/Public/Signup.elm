module Common.Pages.Public.Signup exposing
    ( Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , view
    )

{-| Sign up for an account.

The consent checkbox is there only when the instance has a privacy policy or
terms of service. The links to them stand beside the checkbox rather than in
its label, as the wizard had them: a link inside a label is a click target
inside another, and following it would tick the box.

The form has a honeypot, a second checkbox a person never sees; a
form that arrives with it ticked is treated as sent and goes nowhere.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Users as UsersApi
import Common.Components.Form as FormView
import Common.Components.Form.Field as FormField
import Common.Components.PasswordStrength as PasswordStrength
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.SignupForm as SignupForm exposing (SignupForm)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Public.Components.Consent as Consent
import Common.Pages.Public.Layout as PublicLayout
import Common.Ports.Dom as Dom
import Common.Routes as Routes
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Field
import Gettext exposing (gettext)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (attribute, checked, class, for, id, name, tabindex, type_)
import Html.Events exposing (onCheck)
import Ui.Alert as Alert
import Ui.AppShell as AppShell
import Ui.Link as Link


type alias Model =
    { form : Form FormError SignupForm
    , signingUp : ActionResult String
    , passwordStrength : PasswordStrength.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { form = SignupForm.initEmpty appState
    , signingUp = Unset
    , passwordStrength = PasswordStrength.init appState
    }


type Msg
    = FormMsg Form.Msg
    | PostSignupCompleted (Result ApiError ())
    | PasswordStrengthMsg PasswordStrength.Msg


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        FormMsg formMsg ->
            case ( formMsg, Form.getOutput model.form, PasswordStrength.isChecking model.passwordStrength ) of
                ( Form.Submit, Just signupForm, False ) ->
                    if signupForm.acceptFake then
                        ( { model | signingUp = Success "" }, focusTitle )

                    else
                        ( { model | signingUp = Loading }
                        , Cmd.map cfg.wrapMsg <|
                            UsersApi.postUserPublic appState (SignupForm.encode signupForm) PostSignupCompleted
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

        PostSignupCompleted result ->
            case result of
                Ok _ ->
                    ( { model | signingUp = Success "" }, focusTitle )

                Err error ->
                    ( { model
                        | signingUp = ApiError.toActionResult appState (gettext "Sign up process failed." appState.locale) error
                        , form = FormUtils.setFormErrors appState error model.form
                      }
                    , FormView.focusErrorSummary
                    )


passwordField : AppState -> PasswordStrength.PasswordField SignupForm
passwordField appState =
    { path = "password", validation = SignupForm.validation appState }


{-| The form is replaced by the message, so the button that sent it is gone.
-}
focusTitle : Cmd msg
focusTitle =
    Dom.focus ("#" ++ AppShell.mainHeadingId)


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    case model.signingUp of
        Success _ ->
            Sub.none

        _ ->
            FormView.submitShortcut appState { result = model.signingUp, onSubmit = FormMsg Form.Submit }


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.signingUp of
                Success _ ->
                    [ Alert.toHtml (Alert.success (gettext "Sign up was successful. Check your email for the activation link." appState.locale)) ]

                _ ->
                    [ FormView.view
                        { onSubmit = FormMsg Form.Submit
                        , content =
                            FormView.errorSummary model.signingUp
                                :: List.map (Html.map FormMsg) (formView appState model.passwordStrength model.form)
                                ++ [ FormView.actions appState
                                        { submitLabel = gettext "Sign Up" appState.locale
                                        , result = model.signingUp
                                        , onCancel = Nothing
                                        }
                                   ]
                        }
                    , Link.link { label = gettext "I already have an account" appState.locale, url = Routes.toUrl appState (Routes.publicLogin Nothing) }
                        |> Link.toHtml
                    ]
    in
    PublicLayout.panel appState { title = gettext "Sign Up" appState.locale, content = content, intro = Nothing, aside = Nothing }


formView : AppState -> PasswordStrength.Model -> Form FormError SignupForm -> List (Html Form.Msg)
formView appState passwordStrength form =
    [ FormField.email appState form "email" (gettext "Email" appState.locale)
        |> FormField.withRequired True
        |> FormField.withAutocomplete "email"
        |> FormField.toHtml
    , FormField.input appState form "firstName" (gettext "First name" appState.locale)
        |> FormField.withRequired True
        |> FormField.withAutocomplete "given-name"
        |> FormField.toHtml
    , FormField.input appState form "lastName" (gettext "Last name" appState.locale)
        |> FormField.withRequired True
        |> FormField.withAutocomplete "family-name"
        |> FormField.toHtml
    , FormField.input appState form "affiliation" (gettext "Affiliation" appState.locale)
        |> FormField.withAutocomplete "organization"
        |> FormField.withSuggestions appState.config.users.affiliations
        |> FormField.toHtml
    , FormField.passwordWithStrength appState passwordStrength form "password" (gettext "Password" appState.locale)
        |> FormField.withRequired True
        |> FormField.withAutocomplete "new-password"
        |> FormField.toHtml
    , FormField.password appState form "passwordConfirmation" (gettext "Password again" appState.locale)
        |> FormField.withRequired True
        |> FormField.withAutocomplete "new-password"
        |> FormField.toHtml
    , Consent.formField appState form "accept"
    , honeypot form
    ]


{-| Off-screen and out of the tab order and the accessibility tree, so only a
bot filling in every field ticks it.
-}
honeypot : Form FormError SignupForm -> Html Form.Msg
honeypot form =
    div [ class "c-public-honeypot", attribute "aria-hidden" "true" ]
        [ label [ for "accept2" ]
            [ input
                [ type_ "checkbox"
                , id "accept2"
                , name "accept2"
                , tabindex -1
                , checked (Maybe.withDefault False (Form.getFieldAsBool "acceptFake" form).value)
                , onCheck (Form.Input "acceptFake" Form.Checkbox << Form.Field.Bool)
                ]
                []
            , text "I accept this."
            ]
        ]
