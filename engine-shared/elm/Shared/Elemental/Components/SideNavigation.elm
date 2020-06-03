module Shared.Elemental.Components.SideNavigation exposing
    ( ItemConfig
    , ItemConfigCaret(..)
    , ItemDefaultConfig
    , ItemNestedConfig
    , item
    , itemDefault
    , itemNested
    , projectName
    , view
    , wrapper
    )

import Css exposing (..)
import Css.Global exposing (class, descendants, typeSelector)
import Css.Transitions exposing (transition)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Maybe.Extra as Maybe
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Size as Size
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Transition as Transition
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (colorD05, colorD10, colorL40, px2rem)
import Shared.Html.Styled exposing (emptyNode, fa)


wrapper : List (Attribute msg) -> Html msg -> Html msg -> Html msg
wrapper attributes navigation content =
    let
        contentWrapperStyles =
            [ padding4
                (px2rem Size.headerStack)
                (px2rem Spacing.gridComfortable)
                zero
                (px2rem (Size.sideNavigationWidth + Spacing.gridComfortable))
            , minHeight (calc (vh 100) minus (px2rem Size.navigationHeight))
            ]
    in
    div attributes
        [ navigation
        , div [ css contentWrapperStyles ] [ content ]
        ]


view : List (Attribute msg) -> List (Html msg) -> Html msg
view attributes content =
    let
        styles =
            [ height (calc (vh 100) minus (px2rem Size.navigationHeight))
            , width (px2rem Size.sideNavigationWidth)
            , padding3 (px2rem Size.headerStack) (px2rem Spacing.gridComfortable) (px2rem Spacing.gridComfortable)
            , position fixed
            , left zero
            , top (px2rem Size.navigationHeight)
            , overflowY auto
            ]
    in
    div (css styles :: attributes) content


projectName : Theme -> String -> Html msg
projectName theme nameText =
    let
        styles =
            [ Typography.heading3 theme
            , Spacing.stackLG
            , displayFlex
            , alignItems center
            , descendants
                [ typeSelector "img"
                    [ Spacing.inlineSM
                    , width (px2rem 45)
                    , height (px2rem 45)
                    ]
                , typeSelector "span"
                    [ flexGrow (num 1)
                    , whiteSpace noWrap
                    , overflow hidden
                    , textOverflow ellipsis
                    ]
                ]
            ]
    in
    div [ css styles ]
        [ img [ src "/img/project-icons/1.png" ] []
        , span [] [ text nameText ]
        ]


type ItemConfigCaret
    = CaretNone
    | CaretOpen
    | CaretClosed


type alias ItemConfig =
    { icon : String
    , label : String
    , badge : Maybe Int
    , selected : Bool
    , caret : ItemConfigCaret
    }


item : Theme -> ItemConfig -> List (Html.Attribute msg) -> Html msg
item theme cfg attributes =
    let
        styles =
            if cfg.selected then
                commonStyles theme ++ selectedStyles theme

            else
                commonStyles theme ++ defaultStyles theme
    in
    a (css styles :: attributes)
        [ itemCaret cfg.caret
        , fa cfg.icon
        , itemLabel cfg.label
        , itemBadge theme cfg.badge
        ]


type alias ItemDefaultConfig =
    { icon : String
    , label : String
    , badge : Maybe Int
    , selected : Bool
    }


itemDefault : Theme -> ItemDefaultConfig -> List (Html.Attribute msg) -> Html msg
itemDefault theme cfg =
    item theme
        { icon = cfg.icon
        , label = cfg.label
        , badge = cfg.badge
        , selected = cfg.selected
        , caret = CaretNone
        }


type alias ItemNestedConfig =
    { number : Maybe String
    , label : String
    , badge : Maybe Int
    , selected : Bool
    }


itemNested : Theme -> ItemNestedConfig -> List (Html.Attribute msg) -> Html msg
itemNested theme cfg attributes =
    let
        nestedStyles =
            [ paddingLeft (px2rem Spacing.lg) ]

        styles =
            if cfg.selected then
                commonStyles theme ++ selectedStyles theme ++ nestedStyles

            else
                commonStyles theme ++ defaultStyles theme ++ nestedStyles
    in
    a (css styles :: attributes)
        [ itemNumber cfg.number
        , itemLabel cfg.label
        , itemBadge theme cfg.badge
        ]


commonStyles : Theme -> List Style
commonStyles theme =
    [ Typography.heading3 theme
    , Spacing.stackSM
    , Spacing.insetSquishMD
    , Border.roundedFull
    , important (fontWeight (int 600))
    , textDecoration none
    , position relative
    , displayFlex
    , alignItems center
    , cursor pointer
    , transition
        [ Transition.default Css.Transitions.backgroundColor3
        , Transition.default Css.Transitions.color3
        ]
    , descendants
        [ class "fa"
            [ Spacing.inlineSM
            , width (px2rem 20)
            , textAlign center
            , transition
                [ Transition.default Css.Transitions.color3 ]
            ]
        ]
    ]


defaultStyles : Theme -> List Style
defaultStyles theme =
    [ hover
        [ backgroundColor (colorD05 theme.colors.background) ]
    , descendants
        [ class "fa"
            [ color theme.colors.textLighter
            ]
        ]
    ]


selectedStyles : Theme -> List Style
selectedStyles theme =
    [ important (color (colorD10 theme.colors.primary))
    , backgroundColor (colorL40 theme.colors.primary)
    , descendants
        [ class "fa"
            [ color (colorD10 theme.colors.primary)
            ]
        ]
    ]


itemCaret : ItemConfigCaret -> Html msg
itemCaret caret =
    let
        caretStyles =
            [ position absolute
            , left zero
            , fontSize (pct 80)
            ]
    in
    case caret of
        CaretNone ->
            emptyNode

        CaretClosed ->
            span [ css caretStyles ] [ fa "fas fa-angle-right" ]

        CaretOpen ->
            span [ css caretStyles ] [ fa "fas fa-angle-down" ]


itemNumber : Maybe String -> Html msg
itemNumber mbValue =
    let
        styles =
            [ Spacing.inlineXS
            , display inlineBlock
            , width (px2rem 20)
            , textAlign right
            ]
    in
    span [ css styles ] [ text <| Maybe.withDefault "" mbValue ]


itemLabel : String -> Html msg
itemLabel value =
    let
        styles =
            [ Spacing.inlineSM
            , flexGrow (num 1)
            , whiteSpace noWrap
            , overflow hidden
            , textOverflow ellipsis
            ]
    in
    span [ css styles ] [ text value ]


itemBadge : Theme -> Maybe Int -> Html msg
itemBadge theme mbValue =
    let
        styles =
            [ Typography.copy2inversed theme
            , Spacing.insetSquishSM
            , Border.roundedFull
            , fontWeight bold
            , backgroundColor theme.colors.danger
            , display block
            , lineHeight (num 1)
            ]

        badge value =
            span [ css styles ] [ text <| String.fromInt value ]
    in
    Maybe.unwrap emptyNode badge mbValue
