module Wizard.Common.View.Page exposing
    ( actionResultView
    , actionResultViewWithError
    , error
    , header
    , illustratedMessage
    , illustratedMessageHtml
    , loader
    , success
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, br, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Undraw as Undraw
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


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
        , div [] [ text (gettext "Loading..." appState.locale) ]
        ]


error : AppState -> String -> Html msg
error appState msg =
    illustratedMessage
        { image = Undraw.cancel
        , heading = gettext "Error" appState.locale
        , lines = [ msg ]
        , cy = "error"
        }


success : AppState -> String -> Html msg
success appState =
    message (faSet "_global.success" appState) "success"


message : Html msg -> String -> String -> Html msg
message icon cy msg =
    div [ class "px-4 py-5 bg-light rounded-3 full-page-message", dataCy ("message_" ++ cy) ]
        [ h1 [ class "display-3" ] [ icon ]
        , p [ class "lead" ] [ text msg ]
        ]


illustratedMessage :
    { image : Html msg
    , heading : String
    , lines : List String
    , cy : String
    }
    -> Html msg
illustratedMessage { image, heading, lines, cy } =
    let
        content =
            lines
                |> List.map text
                |> List.intersperse (br [] [])
    in
    illustratedMessageHtml { image = image, heading = heading, content = [ p [] content ], cy = cy }


illustratedMessageHtml :
    { image : Html msg
    , heading : String
    , content : List (Html msg)
    , cy : String
    }
    -> Html msg
illustratedMessageHtml { image, heading, content, cy } =
    div [ class "full-page-illustrated-message", dataCy ("illustrated-message_" ++ cy) ]
        [ image
        , div []
            (h1 [] [ text heading ] :: content)
        ]


actionResultView : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
actionResultView appState viewContent =
    actionResultViewWithError appState viewContent (error appState)


actionResultViewWithError : AppState -> (a -> Html msg) -> (String -> Html msg) -> ActionResult a -> Html msg
actionResultViewWithError appState viewContent viewError actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            loader appState

        Error err ->
            viewError err

        Success result ->
            viewContent result
