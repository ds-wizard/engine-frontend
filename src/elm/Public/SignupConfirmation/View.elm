module Public.SignupConfirmation.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (linkTo)
import Common.Locale exposing (lh, lx)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Public.Routes exposing (Route(..))
import Public.SignupConfirmation.Models exposing (Model)
import Public.SignupConfirmation.Msgs exposing (Msg)
import Routes


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Public.SignupConfirmation.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Public.SignupConfirmation.View"


view : AppState -> Model -> Html Msg
view appState model =
    div [ class "row justify-content-center Public__SignupConfirmation" ]
        [ Page.actionResultView appState (successView appState) model.confirmation ]


successView : AppState -> String -> Html Msg
successView appState _ =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class "fa fa-check" ] [] ]
        , p [ class "lead" ]
            (lh_ "confirmation"
                [ linkTo appState (Routes.PublicRoute LoginRoute) [] [ lx_ "logIn" appState ]
                ]
                appState
            )
        ]
