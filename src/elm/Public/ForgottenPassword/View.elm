module Public.ForgottenPassword.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.View.FormGroup as FormGroup
import Common.View.Forms exposing (formTextAfter)
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs
import Public.Common.View exposing (publicForm)
import Public.ForgottenPassword.Models exposing (ForgottenPasswordForm, Model)
import Public.ForgottenPassword.Msgs exposing (Msg(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    let
        content =
            case model.submitting of
                Success _ ->
                    Page.success "We've sent you a recover link. Follow the instructions in your email."

                _ ->
                    forgottenPasswordForm wrapMsg model
    in
    div [ class "row justify-content-center Public__ForgottenPassword" ]
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
        [ FormGroup.input form "email" "Email"
        , formTextAfter "Enter the email you use to log in and we will send you a recover link."
        ]
