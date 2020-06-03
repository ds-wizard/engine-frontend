module Shared.Elemental.Components.Carousel exposing
    ( PageOptions
    , container
    , page
    , pageCurrent
    , pageNext
    , pagePrevious
    )

import Css exposing (..)
import Css.Transitions exposing (easeInOut, transition)
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (class, css)
import Shared.Elemental.Foundations.Transition as Animation


container : List (Html msg) -> Html msg
container elements =
    let
        style =
            [ width (pct 100)
            , overflowX hidden
            , displayFlex
            ]
    in
    div [ class "carousel", css style ] elements


type PageOptions
    = PageOptions Style


pageCurrent : PageOptions
pageCurrent =
    PageOptions <|
        Css.batch []


pagePrevious : PageOptions
pagePrevious =
    PageOptions <|
        Css.batch [ marginLeft (pct -100) ]


pageNext : PageOptions
pageNext =
    PageOptions <|
        Css.batch []


page : PageOptions -> List (Html msg) -> Html msg
page (PageOptions pageStyle) elemetns =
    let
        style =
            [ pageStyle
            , width (pct 100)
            , flexGrow (num 1)
            , flexShrink zero
            , transition
                [ Animation.slower Css.Transitions.marginLeft3
                ]
            ]
    in
    div [ class "page", css style ] elemetns
