module Shared.Components.Page exposing
    ( actionResultView
    , actionResultViewWithError
    , error
    , header
    , headerWithGuideLink
    , illustratedMessage
    , illustratedMessageHtml
    , loader
    , success
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, br, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Shared.Components.FontAwesome exposing (faSpinner, faSuccess)
import Shared.Components.GuideLink as GuideLink exposing (GuideLinkConfig)
import Shared.Components.Undraw as Undraw


header : String -> List (Html msg) -> Html msg
header title actions =
    div [ class "header" ]
        [ h2 [] [ text title ]
        , headerActions actions
        ]


headerWithGuideLink : GuideLinkConfig -> String -> Html msg
headerWithGuideLink guideLinkConfig title =
    div [ class "header" ]
        [ h2 [] [ text title ]
        , div []
            [ GuideLink.guideLink guideLinkConfig
            ]
        ]


headerActions : List (Html msg) -> Html msg
headerActions actions =
    div [ class "actions" ]
        actions


loader : { a | locale : Gettext.Locale } -> Html msg
loader appState =
    div [ class "full-page-loader" ]
        [ faSpinner
        , div [] [ text (gettext "Loading..." appState.locale) ]
        ]


error : { b | locale : Gettext.Locale } -> String -> Html msg
error appState msg =
    illustratedMessage
        { image = Undraw.cancel
        , heading = gettext "Error" appState.locale
        , lines = [ msg ]
        , cy = "error"
        }


success : String -> Html msg
success =
    message faSuccess "success"


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


actionResultView : { b | locale : Gettext.Locale } -> (a -> Html msg) -> ActionResult a -> Html msg
actionResultView appState viewContent =
    actionResultViewWithError appState viewContent (error appState)


actionResultViewWithError : { b | locale : Gettext.Locale } -> (a -> Html msg) -> (String -> Html msg) -> ActionResult a -> Html msg
actionResultViewWithError appState viewContent viewError actionResult =
    case actionResult of
        Unset ->
            Html.nothing

        Loading ->
            loader appState

        Error err ->
            viewError err

        Success result ->
            viewContent result
