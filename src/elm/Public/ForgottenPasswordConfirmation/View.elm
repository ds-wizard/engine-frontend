module Public.ForgottenPasswordConfirmation.View exposing (formView, signupForm, successView, view)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Html exposing (linkTo)
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs
import Public.Common.View exposing (publicForm)
import Public.ForgottenPasswordConfirmation.Models exposing (..)
import Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(..))
import Public.Routing exposing (Route(..))
import Routing


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    let
        content =
            case model.submitting of
                Success _ ->
                    successView

                _ ->
                    signupForm wrapMsg model
    in
    div [ class "row justify-content-center Public_ForgottenPasswordConfirmation" ]
        [ content ]


signupForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
signupForm wrapMsg model =
    let
        formConfig =
            { title = "Password Recovery"
            , submitMsg = wrapMsg <| FormMsg Form.Submit
            , actionResult = model.submitting
            , submitLabel = "Save"
            , formContent = formView model.form |> Html.map (wrapMsg << FormMsg)
            , link = Nothing
            }
    in
    publicForm formConfig


formView : Form CustomFormError PasswordForm -> Html Form.Msg
formView form =
    div []
        [ FormExtra.text "Enter a new password you want to use to log in."
        , FormGroup.password form "password" "New password"
        , FormGroup.password form "passwordConfirmation" "New password again"
        ]


successView : Html Msgs.Msg
successView =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class "fa fa-check" ] [] ]
        , p [ class "lead" ]
            [ text "Your password was has been changed. You can now "
            , linkTo (Routing.Public Login) [] [ text "log in" ]
            , text "."
            ]
        ]
