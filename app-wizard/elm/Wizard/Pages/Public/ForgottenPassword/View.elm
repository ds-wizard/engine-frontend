module Wizard.Pages.Public.ForgottenPassword.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Common.ForgottenPasswordForm exposing (ForgottenPasswordForm)
import Wizard.Pages.Public.Common.View exposing (publicForm)
import Wizard.Pages.Public.ForgottenPassword.Models exposing (Model)
import Wizard.Pages.Public.ForgottenPassword.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.submitting of
                Success _ ->
                    Page.success <| gettext "Check your email. If you have an account, you should receive an email with instructions on how to reset your password." appState.locale

                _ ->
                    forgottenPasswordForm appState model
    in
    div [ class "row justify-content-center Public__ForgottenPassword" ]
        [ content ]


forgottenPasswordForm : AppState -> Model -> Html Msg
forgottenPasswordForm appState model =
    let
        formConfig =
            { title = gettext "Forgotten Password" appState.locale
            , submitMsg = FormMsg Form.Submit
            , actionResult = model.submitting
            , submitLabel = gettext "Recover" appState.locale
            , formContent = formView appState model.form |> Html.map FormMsg
            , link = Nothing
            }
    in
    publicForm formConfig


formView : AppState -> Form FormError ForgottenPasswordForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState.locale form "email" <| gettext "Email" appState.locale
        , FormExtra.textAfter <| gettext "Enter the email you use to log in and we will send you a recover link." appState.locale
        ]
