module Public.ForgottenPassword.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (fullPageError)
import Common.View.Forms exposing (errorView, inputGroup, submitButton)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Msgs
import Public.ForgottenPassword.Models exposing (ForgottenPasswordForm, Model)
import Public.ForgottenPassword.Msgs exposing (Msg(FormMsg))


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
    div [ class "Public__ForgottenPassword" ]
        [ content ]


signupForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
signupForm wrapMsg model =
    form [ onSubmit (wrapMsg <| FormMsg Form.Submit), class "well col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-6 col-md-offset-3 col-lg-4 col-lg-offset-4" ]
        [ fieldset []
            [ legend [] [ text "Forgotten Password" ]
            , submitError model.submitting
            , formView model.form |> Html.map (wrapMsg << FormMsg)
            , div [ class "form-actions" ]
                [ submitButton ( "Recover", model.submitting )
                ]
            ]
        ]


formView : Form CustomFormError ForgottenPasswordForm -> Html Form.Msg
formView form =
    div []
        [ inputGroup form "email" "Email"
        , p [ class "help-block help-block-after" ]
            [ text "Enter the email you use to log in and we will send you a recover link." ]
        ]


successView : Html Msgs.Msg
successView =
    fullPageError "fa-check" "We've sent you a recover link. Follow the instructions in your email."


submitError : ActionResult String -> Html Msgs.Msg
submitError result =
    case result of
        Error err ->
            errorView err

        _ ->
            emptyNode
