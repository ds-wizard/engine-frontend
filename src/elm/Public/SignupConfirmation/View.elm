module Public.SignupConfirmation.View exposing (view)

import Common.Html exposing (linkTo)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Public.Routing exposing (Route(..))
import Public.SignupConfirmation.Models exposing (Model)
import Routing


view : Model -> Html Msgs.Msg
view model =
    div [ class "row justify-content-center Public__SignupConfirmation" ]
        [ Page.actionResultView successView model.confirmation ]


successView : String -> Html Msgs.Msg
successView _ =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class "fa fa-check" ] [] ]
        , p [ class "lead" ]
            [ text "Your email was successfully confirmed. You can now "
            , linkTo (Routing.Public Login) [] [ text "log in" ]
            , text "."
            ]
        ]
