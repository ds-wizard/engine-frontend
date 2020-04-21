module Shared.Elemental.Components.ProgressBar exposing
    ( ItemOptions
    , container
    , item
    )

import Css exposing (..)
import Css.Global exposing (class, descendants)
import Css.Transitions exposing (transition)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Shadow as Shadow
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Transition as Animation
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Html.Styled exposing (fa)


container : Theme -> Int -> List (ItemOptions -> Html msg) -> Html msg
container theme currentItem items =
    let
        styles =
            [ Spacing.stackLG ]
    in
    div [ css styles ]
        [ itemList theme currentItem items
        , progressBar theme currentItem (List.length items)
        ]


itemList : Theme -> Int -> List (ItemOptions -> Html msg) -> Html msg
itemList theme currentItem items =
    let
        styles =
            [ Spacing.stackSM
            , displayFlex
            ]

        renderItem index toItem =
            let
                itemOptions =
                    if index < currentItem - 1 then
                        itemPrevious

                    else if index > currentItem - 1 then
                        itemNext

                    else
                        itemCurrent
            in
            toItem (itemOptions theme)
    in
    div [ css styles ] (List.indexedMap renderItem items)


progressBar : Theme -> Int -> Int -> Html msg
progressBar theme currentItem itemCount =
    let
        fCurrentItem =
            toFloat currentItem

        fItemCount =
            toFloat itemCount

        size =
            fCurrentItem / fItemCount * 100 - (100 / (2 * fItemCount))

        wrapperStyles =
            [ position relative
            , width (pct 100)
            , height (px2rem 15)
            ]

        fillStyles =
            [ Border.roundedFull
            , position absolute
            , left zero
            , top (px2rem 6)
            , width (pct size)
            , height (px2rem 3)
            , backgroundColor theme.colors.primary
            , transition
                [ Animation.slower Css.Transitions.width3
                ]
            ]

        handleStyles =
            [ Shadow.xs Shadow.colorPrimary theme
            , position absolute
            , left (pct size)
            , top (px2rem 4)
            , width (px2rem 7)
            , height (px2rem 7)
            , marginLeft (px2rem -3)
            , backgroundColor theme.colors.primary
            , borderRadius (pct 100)
            , transition
                [ Animation.slower Css.Transitions.left3 ]
            ]
    in
    div [ css wrapperStyles ]
        [ div [ css fillStyles ] []
        , div [ css handleStyles ] []
        ]


type ItemOptions
    = ItemOptions Style


itemPrevious : Theme -> ItemOptions
itemPrevious theme =
    ItemOptions <|
        Css.batch [ Typography.copy1lighter theme ]


itemCurrent : Theme -> ItemOptions
itemCurrent theme =
    ItemOptions <|
        Css.batch [ Typography.heading3 theme ]


itemNext : Theme -> ItemOptions
itemNext theme =
    ItemOptions <|
        Css.batch [ Typography.copy1light theme ]


item : String -> String -> ItemOptions -> Html msg
item icon itemText (ItemOptions itemStyle) =
    let
        styles =
            [ itemStyle
            , textAlign center
            , flex3 (num 1) (num 1) (px 0)
            , descendants
                [ class "fa"
                    [ fontSize (rem 1)
                    , Spacing.inlineSM
                    ]
                ]
            ]
    in
    div [ css styles ]
        [ fa icon
        , text itemText
        ]
