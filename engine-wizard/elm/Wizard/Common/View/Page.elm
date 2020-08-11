module Wizard.Common.View.Page exposing
    ( actionResultView
    , error
    , header
    , illustratedMessage
    , illustratedMessageHtml
    , loader
    , message
    , success
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, br, div, h1, h2, img, p, text)
import Html.Attributes exposing (class, src)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.View.Page"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.View.Page"


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
        [ faSet "_global.spinner" appState
        , div [] [ lx_ "loader.loading" appState ]
        ]


error : AppState -> String -> Html msg
error appState msg =
    illustratedMessage
        { image = "cancel"
        , heading = l_ "error.heading" appState
        , lines = [ msg ]
        }


success : AppState -> String -> Html msg
success appState =
    message (faSet "_global.success" appState)


message : Html msg -> String -> Html msg
message icon msg =
    div [ class "jumbotron full-page-message" ]
        [ h1 [ class "display-3" ] [ icon ]
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
    illustratedMessageHtml { image = image, heading = heading, content = [ p [] content ] }


illustratedMessageHtml :
    { image : String
    , heading : String
    , content : List (Html msg)
    }
    -> Html msg
illustratedMessageHtml { image, heading, content } =
    div [ class "full-page-illustrated-message" ]
        [ img [ src <| "/img/illustrations/undraw_" ++ image ++ ".svg" ] []
        , div []
            (h1 [] [ text heading ] :: content)
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
