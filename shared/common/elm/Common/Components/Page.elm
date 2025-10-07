module Common.Components.Page exposing
    ( IllustratedMessageConfig
    , IllustratedMessageHtmlConfig
    , actionResultView
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
import Common.Components.FontAwesome exposing (faSpinner, faSuccess)
import Common.Components.GuideLink as GuideLink exposing (GuideLinkConfig)
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, br, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html


header : String -> List (Html msg) -> Html msg
header title actions =
    div [ class "d-flex justify-content-between align-items-center  mb-3" ]
        [ h2 [] [ text title ]
        , div [] actions
        ]


headerWithGuideLink : GuideLinkConfig -> String -> Html msg
headerWithGuideLink guideLinkConfig title =
    header title [ GuideLink.guideLink guideLinkConfig ]


loader : { a | locale : Gettext.Locale } -> Html msg
loader appState =
    div [ class "page-loader" ]
        [ faSpinner
        , div [] [ text (gettext "Loading..." appState.locale) ]
        ]


error : { a | locale : Gettext.Locale } -> String -> Html msg
error appState msg =
    illustratedMessage
        { illustration = Undraw.cancel
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


type alias IllustratedMessageConfig msg =
    { illustration : Html msg
    , heading : String
    , lines : List String
    , cy : String
    }


illustratedMessage : IllustratedMessageConfig msg -> Html msg
illustratedMessage cfg =
    let
        content =
            cfg.lines
                |> List.map text
                |> List.intersperse (br [] [])
    in
    illustratedMessageHtml
        { illustration = cfg.illustration
        , heading = cfg.heading
        , content = [ p [ class "font-lg" ] content ]
        , cy = cfg.cy
        }


type alias IllustratedMessageHtmlConfig msg =
    { illustration : Html msg
    , heading : String
    , content : List (Html msg)
    , cy : String
    }


illustratedMessageHtml : IllustratedMessageHtmlConfig msg -> Html msg
illustratedMessageHtml cfg =
    div
        [ class "container mt-5"
        , dataCy ("illustrated-message_" ++ cfg.cy)
        ]
        [ div [ class "row justify-content-center" ]
            [ div [ class "col-4 me-4" ] [ cfg.illustration ]
            , div [ class "col-4 d-flex flex-column justify-content-center align-items-start" ]
                (h1 [] [ text cfg.heading ] :: cfg.content)
            ]
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
