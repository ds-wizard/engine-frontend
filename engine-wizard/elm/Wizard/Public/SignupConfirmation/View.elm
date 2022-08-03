module Wizard.Public.SignupConfirmation.View exposing (view)

import Html exposing (Html, div, h1, p)
import Html.Attributes exposing (class)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (lh, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Page as Page
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
    div [ class "px-4 py-5 bg-light rounded-3e", dataCy "message_success" ]
        [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
        , p [ class "lead" ]
            (lh_ "confirmation"
                [ linkTo appState (Routes.publicLogin Nothing) [ class "btn btn-primary ms-1" ] [ lx_ "logIn" appState ]
                ]
                appState
            )
        ]
