module Wizard.Public.ForgottenPasswordConfirmation.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (Html, div, h1, p)
import Html.Attributes exposing (class)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Public.Common.PasswordForm exposing (PasswordForm)
import Wizard.Public.Common.View exposing (publicForm)
import Wizard.Public.ForgottenPasswordConfirmation.Models exposing (Model)
import Wizard.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.ForgottenPasswordConfirmation.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Public.ForgottenPasswordConfirmation.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Public.ForgottenPasswordConfirmation.View"


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
            { title = l_ "form.title" appState
            , submitMsg = FormMsg Form.Submit
            , actionResult = model.submitting
            , submitLabel = l_ "form.submit" appState
            , formContent = formView appState model.form |> Html.map FormMsg
            , link = Nothing
            }
    in
    publicForm appState formConfig


formView : AppState -> Form FormError PasswordForm -> Html Form.Msg
formView appState form =
    div []
        [ FormExtra.text <| l_ "form.text" appState
        , FormGroup.passwordWithStrength appState form "password" <| l_ "form.password" appState
        , FormGroup.password appState form "passwordConfirmation" <| l_ "form.passwordConfirmation" appState
        ]


successView : AppState -> Html Msg
successView appState =
    div [ class "px-4 py-5 bg-light rounded-3", dataCy "message_success" ]
        [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (lh_ "success.message"
                [ linkTo appState (Routes.publicLogin Nothing) [ dataCy "login-link", class "btn btn-primary ms-1" ] [ lx_ "success.logIn" appState ]
                ]
                appState
            )
        ]
