module Public.Login.View exposing (view)

import Common.Html exposing (linkTo)
import Common.Types exposing (ActionResult)
import Common.View.Forms exposing (actionButton, formResultView, submitButton)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, for, id, placeholder, type_)
import Html.Events exposing (..)
import Msgs
import Public.Common.View exposing (publicForm)
import Public.Login.Models exposing (Model)
import Public.Login.Msgs exposing (Msg(..))
import Public.Routing exposing (Route(ForgottenPassword))
import Routing exposing (Route(Public))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "row justify-content-center" ]
        [ loginForm wrapMsg model ]


loginForm : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
loginForm wrapMsg model =
    let
        formContent =
            div []
                [ div [ class "form-group" ]
                    [ label [ for "email" ] [ text "Email" ]
                    , input [ onInput (wrapMsg << Email), id "email", type_ "text", class "form-control", placeholder "Email" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "password" ] [ text "Password" ]
                    , input [ onInput (wrapMsg << Password), id "password", type_ "password", class "form-control", placeholder "Password" ] []
                    ]
                ]

        formConfig =
            { title = "Log in"
            , submitMsg = wrapMsg Login
            , actionResult = model.loggingIn
            , submitLabel = "Log in"
            , formContent = formContent
            , link = Just ( Public ForgottenPassword, "Forgot your password?" )
            }
    in
    publicForm formConfig
