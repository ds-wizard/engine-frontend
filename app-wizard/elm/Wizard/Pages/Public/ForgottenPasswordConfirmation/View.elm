module Wizard.Pages.Public.ForgottenPasswordConfirmation.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Components.FontAwesome exposing (faSuccess)
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import String.Format as String
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Common.PasswordForm exposing (PasswordForm)
import Wizard.Pages.Public.Common.View exposing (publicForm)
import Wizard.Pages.Public.ForgottenPasswordConfirmation.Models exposing (Model)
import Wizard.Pages.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.submitting of
                Success _ ->
                    successView appState

                _ ->
                    signupForm appState model
    in
    div [ class "row justify-content-center Public_ForgottenPasswordConfirmation" ]
        [ content ]


signupForm : AppState -> Model -> Html Msg
signupForm appState model =
    let
        formConfig =
            { title = gettext "Password Recovery" appState.locale
            , submitMsg = FormMsg Form.Submit
            , actionResult = model.submitting
            , submitLabel = gettext "Save" appState.locale
            , formContent = formView appState model.form |> Html.map FormMsg
            , link = Nothing
            }
    in
    publicForm formConfig


formView : AppState -> Form FormError PasswordForm -> Html Form.Msg
formView appState form =
    div []
        [ FormExtra.text <| gettext "Enter a new password you want to use to log in." appState.locale
        , FormGroup.passwordWithStrength appState.locale form "password" <| gettext "New password" appState.locale
        , FormGroup.password appState.locale form "passwordConfirmation" <| gettext "New password again" appState.locale
        ]


successView : AppState -> Html Msg
successView appState =
    div [ class "px-4 py-5 bg-light rounded-3", dataCy "message_success" ]
        [ h1 [ class "display-3" ] [ faSuccess ]
        , p [ class "lead" ]
            (String.formatHtml
                (gettext "Your password has been changed. You can now %s." appState.locale)
                [ linkTo (Routes.publicLogin Nothing)
                    [ dataCy "login-link", class "btn btn-primary ms-1" ]
                    [ text (gettext "log in" appState.locale) ]
                ]
            )
        ]
