module Public.Signup.View exposing (view)

import Common.Form.Validate exposing (CustomFormError)
import Common.Html exposing (emptyNode)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (fullPageError)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class)
import Msgs
import Public.Signup.Models exposing (..)
import Public.Signup.Msgs exposing (Msg(FormMsg))


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
    div [ class "public__signup" ]
        [ content ]


signupForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
signupForm wrapMsg model =
    div [ class "well col-xs-10 col-xs-offset-1 col-sm-8 col-sm-offset-2 col-md-6 col-md-offset-3 col-lg-4 col-lg-offset-4" ]
        [ fieldset []
            [ legend [] [ text "Sign up" ]
            , signupError model.signingUp
            , formView model.form |> Html.map (wrapMsg << FormMsg)
            , formActionOnly ( "Sign up", model.signingUp, wrapMsg <| FormMsg Form.Submit )
            ]
        ]


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
    fullPageError "fa-check" "Sign up was successful. Check your email for activation link."


signupError : ActionResult String -> Html Msgs.Msg
signupError result =
    case result of
        Error err ->
            errorView err

        _ ->
            emptyNode
