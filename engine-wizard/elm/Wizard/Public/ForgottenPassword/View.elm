module Wizard.Public.ForgottenPassword.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Shared.Locale exposing (l, lg)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Page as Page
import Wizard.Public.Common.ForgottenPasswordForm exposing (ForgottenPasswordForm)
import Wizard.Public.Common.View exposing (publicForm)
import Wizard.Public.ForgottenPassword.Models exposing (Model)
import Wizard.Public.ForgottenPassword.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.ForgottenPassword.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.submitting of
                Success _ ->
                    Page.success appState <| l_ "recoveryLinkSent" appState

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
