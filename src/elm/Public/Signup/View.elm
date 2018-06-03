module Public.Signup.View exposing (view)

import Common.Form exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (fullPageMessage)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs
import Public.Common.View exposing (publicForm)
import Public.Routing exposing (Route(Login))
import Public.Signup.Models exposing (..)
import Public.Signup.Msgs exposing (Msg(FormMsg))
import Routing exposing (Route(Public))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    let
        content =
            case model.signingUp of
                Success _ ->
                    successView

                _ ->
                    signupForm wrapMsg model
    in
    div [ class "row justify-content-center" ]
        [ content ]


signupForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
signupForm wrapMsg model =
    let
        formConfig =
            { title = "Sign up"
            , submitMsg = wrapMsg <| FormMsg Form.Submit
            , actionResult = model.signingUp
            , submitLabel = "Sign up"
            , formContent = formView model.form |> Html.map (wrapMsg << FormMsg)
            , link = Just ( Public Login, "I already have an account" )
            }
    in
    publicForm formConfig


formView : Form CustomFormError SignupForm -> Html Form.Msg
formView form =
    div []
        [ inputGroup form "email" "Email"
        , inputGroup form "name" "Name"
        , inputGroup form "surname" "Surname"
        , passwordGroup form "password" "Password"
        , passwordGroup form "passwordConfirmation" "Password again"
        ]


successView : Html Msgs.Msg
successView =
    fullPageMessage "fa-check" "Sign up was successful. Check your email for activation link."
