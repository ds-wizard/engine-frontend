module Wizard.Public.SignupConfirmation.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Locale exposing (lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (faSet, linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Public.Routes exposing (Route(..))
import Wizard.Public.SignupConfirmation.Models exposing (Model)
import Wizard.Public.SignupConfirmation.Msgs exposing (Msg)
import Wizard.Routes as Routes


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Public.SignupConfirmation.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Public.SignupConfirmation.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "row justify-content-center Public__SignupConfirmation" ]
        [ Page.actionResultView appState (successView appState) model.confirmation ]


successView : AppState -> String -> Html Msg
successView appState _ =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (lh_ "confirmation"
                [ linkTo appState (Routes.PublicRoute (LoginRoute Nothing)) [] [ lx_ "logIn" appState ]
                ]
                appState
            )
        ]
