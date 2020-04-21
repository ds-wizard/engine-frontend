module Shared.Elemental.Foundations.Grid exposing
    ( Grid
    , colSeparated
    , colTextRight
    , colVerticalCenter
    , comfortable
    , compact
    , containerExtraIndented
    , containerFluid
    , containerFullHeight
    , containerIndented
    , containerLimited
    , containerLimitedSmall
    , cozy
    )

import Css exposing (..)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (class, css)
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)


type alias Grid msg =
    { container : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , block : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , col : Float -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , colOffset : ( Float, Float ) -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
    }


comfortable : Grid msg
comfortable =
    { container = container Spacing.gridComfortable
    , block = block Spacing.gridComfortable
    , row = row Spacing.gridComfortable
    , col = col Spacing.gridComfortable
    , colOffset = colOffset Spacing.gridComfortable
    }


cozy : Grid msg
cozy =
    { container = container Spacing.gridCozy
    , block = block Spacing.gridCozy
    , row = row Spacing.gridCozy
    , col = col Spacing.gridCozy
    , colOffset = colOffset Spacing.gridCozy
    }


compact : Grid msg
compact =
    { container = container Spacing.gridCompact
    , block = block Spacing.gridCompact
    , row = row Spacing.gridCompact
    , col = col Spacing.gridCompact
    , colOffset = colOffset Spacing.gridCompact
    }


containerFluid : Html.Attribute msg
containerFluid =
    css
        [ width (pct 100)
        ]


containerLimited : Html.Attribute msg
containerLimited =
    css
        [ maxWidth (px 1280)
        , margin2 zero auto
        ]


containerLimitedSmall : Html.Attribute msg
containerLimitedSmall =
    css
        [ maxWidth (px 960)
        , margin2 zero auto
        ]


containerFullHeight : Html.Attribute msg
containerFullHeight =
    css [ height (calc (vh 100) minus (px2rem 50)) ]


containerIndented : Html.Attribute msg
containerIndented =
    css
        [ important (marginTop (px2rem Spacing.lg)) ]


containerExtraIndented : Html.Attribute msg
containerExtraIndented =
    css
        [ important (marginTop (px2rem Spacing.xxl)) ]


container : Float -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
container spacing attributes =
    let
        style =
            []
    in
    div (class "container" :: css style :: attributes)


block : Float -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
block spacing attributes =
    let
        style =
            [ margin2 zero (px2rem (-spacing / 2)) ]
    in
    div (class "block" :: css style :: attributes)


row : Float -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
row spacing attributes =
    let
        style =
            [ displayFlex
            , width (pct 100)
            ]
    in
    div (class "row" :: css style :: attributes)


colTextRight : Html.Attribute msg
colTextRight =
    css [ textAlign right ]


colVerticalCenter : Html.Attribute msg
colVerticalCenter =
    css
        [ displayFlex
        , flexDirection column
        , justifyContent center
        , alignItems flexStart
        ]


colSeparated : Theme -> Html.Attribute msg
colSeparated theme =
    css
        [ borderLeft3 (px 1) solid theme.colors.border
        , paddingLeft (px2rem Spacing.lg)
        ]


col : Float -> Float -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
col spacing n attributes =
    let
        pctWidth =
            n / 12 * 100

        style =
            [ width (calc (pct pctWidth) minus (px2rem spacing))
            , margin2 zero (px2rem (spacing / 2))
            ]
    in
    div (class "col" :: css style :: attributes)


colOffset : Float -> ( Float, Float ) -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
colOffset spacing ( o, n ) attributes =
    let
        pctWidth =
            n / 12 * 100

        pctOffset =
            o / 12 * 100

        style =
            [ width (calc (pct pctWidth) minus (px2rem spacing))
            , margin4
                zero
                (px2rem (spacing / 2))
                zero
                (calc (pct pctOffset) plus (px2rem (spacing / 2)))
            ]
    in
    div (class "col" :: css style :: attributes)
