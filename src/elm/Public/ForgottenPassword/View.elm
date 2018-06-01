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
import Public.Common.View exposing (publicForm)
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
                    forgottenPasswordForm wrapMsg model
    in
    div [ class "row justify-content-center" ]
        [ content ]


forgottenPasswordForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
forgottenPasswordForm wrapMsg model =
    let
        formConfig =
            { title = "Forgotten Password"
            , submitMsg = wrapMsg <| FormMsg Form.Submit
            , actionResult = model.submitting
            , submitLabel = "Recover"
            , formContent = formView model.form |> Html.map (wrapMsg << FormMsg)
            , link = Nothing
            }
    in
    publicForm formConfig


formView : Form CustomFormError ForgottenPasswordForm -> Html Form.Msg
formView form =
    div []
        [ inputGroup form "email" "Email"
        , p [ class "form-text text-muted form-text-after" ]
            [ text "Enter the email you use to log in and we will send you a recover link." ]
        ]


successView : Html Msgs.Msg
successView =
    fullPageError "fa-check" "We've sent you a recover link. Follow the instructions in your email."
