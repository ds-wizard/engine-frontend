module Wizard.Public.ForgottenPasswordConfirmation.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Shared.Locale exposing (l, lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Html exposing (faSet, linkTo)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Public.Common.PasswordForm exposing (PasswordForm)
import Wizard.Public.Common.View exposing (publicForm)
import Wizard.Public.ForgottenPasswordConfirmation.Models exposing (..)
import Wizard.Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))
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


formView : AppState -> Form CustomFormError PasswordForm -> Html Form.Msg
formView appState form =
    div []
        [ FormExtra.text <| l_ "form.text" appState
        , FormGroup.password appState form "password" <| l_ "form.password" appState
        , FormGroup.password appState form "passwordConfirmation" <| l_ "form.passwordConfirmation" appState
        ]


successView : AppState -> Html Msg
successView appState =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (lh_ "success.message"
                [ linkTo appState (Routes.PublicRoute (LoginRoute Nothing)) [] [ lx_ "success.logIn" appState ]
                ]
                appState
            )
        ]
