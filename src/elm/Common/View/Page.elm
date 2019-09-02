module Common.View.Page exposing
    ( actionResultView
    , error
    , header
    , illustratedMessage
    , loader
    , message
    , success
    )

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa)
import Common.Locale exposing (l, lx)
import Html exposing (Html, br, div, h1, h2, img, p, text)
import Html.Attributes exposing (class, src)


l_ : String -> AppState -> String
l_ =
    l "Common.View.Page"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Common.View.Page"


header : String -> List (Html msg) -> Html msg
header title actions =
    div [ class "header" ]
        [ h2 [] [ text title ]
        , headerActions actions
        ]


headerActions : List (Html msg) -> Html msg
headerActions actions =
    div [ class "actions" ]
        actions


loader : AppState -> Html msg
loader appState =
    div [ class "full-page-loader" ]
        [ fa "spinner fa-spin"
        , div [] [ lx_ "loader.loading" appState ]
        ]


error : AppState -> String -> Html msg
error appState msg =
    illustratedMessage
        { image = "cancel"
        , heading = l_ "error.heading" appState
        , lines = [ msg ]
        }


success : String -> Html msg
success =
    message "check"


message : String -> String -> Html msg
message icon msg =
    div [ class "jumbotron full-page-message" ]
        [ h1 [ class "display-3" ] [ fa icon ]
        , p [ class "lead" ] [ text msg ]
        ]


illustratedMessage :
    { image : String
    , heading : String
    , lines : List String
    }
    -> Html msg
illustratedMessage { image, heading, lines } =
    let
        content =
            lines
                |> List.map text
                |> List.intersperse (br [] [])
    in
    div [ class "full-page-illustrated-message" ]
        [ img [ src <| "/img/illustrations/undraw_" ++ image ++ ".svg" ] []
        , div []
            [ h1 [] [ text heading ]
            , p [] content
            ]
        ]


actionResultView : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
actionResultView appState viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            loader appState

        Error err ->
            error appState err

        Success result ->
            viewContent result
