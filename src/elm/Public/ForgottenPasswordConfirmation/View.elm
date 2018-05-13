module Public.ForgottenPasswordConfirmation.View exposing (..)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode, linkTo)
import Common.Types exposing (ActionResult(..))
import Common.View.Forms exposing (errorView, passwordGroup, submitButton)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Msgs
import Public.ForgottenPasswordConfirmation.Models exposing (..)
import Public.ForgottenPasswordConfirmation.Msgs exposing (Msg(FormMsg))
import Public.Routing exposing (Route(Login))
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
    div [ class "public__forgottenPassword" ]
        [ content ]


signupForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
signupForm wrapMsg model =
    form [ onSubmit (wrapMsg <| FormMsg Form.Submit), class "well col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-6 col-md-offset-3 col-lg-4 col-lg-offset-4" ]
        [ fieldset []
            [ legend [] [ text "Password Recovery" ]
            , submitError model.submitting
            , formView model.form |> Html.map (wrapMsg << FormMsg)
            , div [ class "form-actions public__forgottenPassword__formButtons" ]
                [ submitButton ( "Save", model.submitting )
                ]
            ]
        ]


formView : Form CustomFormError PasswordForm -> Html Form.Msg
formView form =
    div []
        [ p [ class "help-block" ]
            [ text "Enter a new password you want to use to log in." ]
        , passwordGroup form "password" "New password"
        , passwordGroup form "passwordConfirmation" "New password again"
        ]


successView : Html Msgs.Msg
successView =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class "fa fa-check" ] [] ]
        , p []
            [ text "Your password was has been changed. You can now "
            , linkTo (Routing.Public Login) [] [ text "log in" ]
            , text "."
            ]
        ]


submitError : ActionResult String -> Html Msgs.Msg
submitError result =
    case result of
        Error err ->
            errorView err

        _ ->
            emptyNode
