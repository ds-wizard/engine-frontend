module Wizard.Public.Login.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, for, id, placeholder, type_)
import Html.Events exposing (..)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (l, lg, lgx)
import Wizard.Public.Common.View exposing (publicForm)
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Public.Login.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "row justify-content-center Public__Login" ]
        [ loginForm appState model ]


loginForm : AppState -> Model -> Html Msg
loginForm appState model =
    let
        formContent =
            div []
                [ div [ class "form-group" ]
                    [ label [ for "email" ] [ lgx "user.email" appState ]
                    , input [ onInput Email, id "email", type_ "text", class "form-control", placeholder <| lg "user.email" appState ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ for "password" ] [ lgx "user.password" appState ]
                    , input [ onInput Password, id "password", type_ "password", class "form-control", placeholder <| lg "user.password" appState ] []
                    ]
                ]

        formConfig =
            { title = l_ "form.title" appState
            , submitMsg = DoLogin
            , actionResult = model.loggingIn
            , submitLabel = l_ "form.submit" appState
            , formContent = formContent
            , link = Just ( Routes.PublicRoute ForgottenPasswordRoute, l_ "form.link" appState )
            }
    in
    publicForm appState formConfig
