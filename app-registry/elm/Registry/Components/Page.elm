module Registry.Components.Page exposing (illustratedMessage, view)

import ActionResult exposing (ActionResult)
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, div, h1, i, p, text)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Registry.Data.AppState exposing (AppState)


view : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
view appState viewPage actionResult =
    case actionResult of
        ActionResult.Unset ->
            Html.nothing

        ActionResult.Loading ->
            Html.div [ class "page-loader my-5" ]
                [ i [ class "fas fa-spinner fa-spin" ] []
                , div [ class "mt-2" ] [ text (gettext "Loading..." appState.locale) ]
                ]

        ActionResult.Success a ->
            viewPage a

        ActionResult.Error e ->
            div [ class "page-error d-flex flex-lg-row flex-column-reverse justify-content-center align-items-center my-5" ]
                [ Undraw.cancel
                , div [ class "ms-5 mb-5" ]
                    [ h1 [] [ text (gettext "Error" appState.locale) ]
                    , p [ class "fs-4" ] [ text e ]
                    ]
                ]


illustratedMessage :
    { image : Html msg
    , heading : String
    , msg : String
    }
    -> Html msg
illustratedMessage { image, heading, msg } =
    div [ class "page-illustrated-message" ]
        [ image
        , div []
            [ h1 [] [ text heading ]
            , p [] [ text msg ]
            ]
        ]
