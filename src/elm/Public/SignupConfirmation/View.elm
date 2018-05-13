module Public.SignupConfirmation.View exposing (..)

import Common.Html exposing (emptyNode, linkTo)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs
import Public.Routing exposing (Route(Login))
import Public.SignupConfirmation.Models exposing (Model)
import Routing


view : Model -> Html Msgs.Msg
view model =
    let
        content =
            case model.confirmation of
                Unset ->
                    emptyNode

                Loading ->
                    fullPageLoader

                Error err ->
                    defaultFullPageError err

                Success _ ->
                    successView
    in
    div [ class "Public__SignupConfirmation" ]
        [ content ]


successView : Html Msgs.Msg
successView =
    div [ class "jumbotron full-page-error" ]
        [ h1 [ class "display-3" ] [ i [ class "fa fa-check" ] [] ]
        , p []
            [ text "Your email was successfully confirmed. You can now "
            , linkTo (Routing.Public Login) [] [ text "log in" ]
            , text "."
            ]
        ]
