module Registry.Common.View.Page exposing (actionResultView, illustratedMessage)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, h1, p, text)
import Html.Attributes exposing (class)
import Shared.Undraw as Undraw


actionResultView : (a -> Html msg) -> ActionResult a -> Html msg
actionResultView view actionResult =
    case actionResult of
        Success a ->
            view a

        Error err ->
            error err

        _ ->
            loader


loader : Html msg
loader =
    div [ class "text-center animation-fade-in" ]
        [ div [ class "spinner-border spinner-border-lg" ] [] ]


error : String -> Html msg
error err =
    illustratedMessage
        { image = Undraw.cancel
        , heading = "Error"
        , msg = err
        }


illustratedMessage :
    { image : Html msg
    , heading : String
    , msg : String
    }
    -> Html msg
illustratedMessage { image, heading, msg } =
    div [ class "full-page-illustrated-message" ]
        [ image
        , div []
            [ h1 [] [ text heading ]
            , p [] [ text msg ]
            ]
        ]
