module Public.ForgottenPassword.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Locale exposing (l, lg)
import Common.View.FormExtra as FormExtra
import Common.View.FormGroup as FormGroup
import Common.View.Page as Page
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Public.Common.ForgottenPasswordForm exposing (ForgottenPasswordForm)
import Public.Common.View exposing (publicForm)
import Public.ForgottenPassword.Models exposing (Model)
import Public.ForgottenPassword.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Public.ForgottenPassword.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.submitting of
                Success _ ->
                    Page.success <| l_ "recoveryLinkSent" appState

                _ ->
                    forgottenPasswordForm appState model
    in
    div [ class "row justify-content-center Public__ForgottenPassword" ]
        [ content ]


forgottenPasswordForm : AppState -> Model -> Html Msg
forgottenPasswordForm appState model =
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


formView : AppState -> Form CustomFormError ForgottenPasswordForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "email" <| lg "user.email" appState
        , FormExtra.textAfter <| l_ "form.email.description" appState
        ]
