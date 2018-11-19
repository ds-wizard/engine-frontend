module Public.Login.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, disabled, for, id, placeholder, type_)
import Html.Events exposing (..)
import Msgs
import Public.Common.View exposing (publicForm)
import Public.Login.Models exposing (Model)
import Public.Login.Msgs exposing (Msg(..))
import Public.Routing exposing (Route(..))
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "row justify-content-center Public__Login" ]
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
            , submitMsg = wrapMsg DoLogin
            , actionResult = model.loggingIn
            , submitLabel = "Log in"
            , formContent = formContent
            , link = Just ( Public ForgottenPassword, "Forgot your password?" )
            }
    in
    publicForm formConfig
