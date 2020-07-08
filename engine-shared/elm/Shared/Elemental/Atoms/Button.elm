module Shared.Elemental.Atoms.Button exposing
    ( actionResultPrimary
    , colorful
    , inline
    , link
    , primary
    , primaryLink
    , primaryLoader
    )

import ActionResult exposing (ActionResult)
import Css exposing (..)
import Css.Global exposing (adjacentSiblings, class, descendants, typeSelector)
import Css.Transitions exposing (transition)
import Html.Styled as Html exposing (Html, a, button)
import Html.Styled.Attributes as Attributes exposing (css)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Shadow as Shadow
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Transition as Transition
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (colorD05, colorL10, colorL20, colorL40, px2rem)
import Shared.Html.Styled exposing (fa)


primary : Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
primary theme attributes content =
    let
        styles =
            [ Typography.heading3inversed theme
            , buttonSharedStyle
            , buttonFilledStyle theme.colors.primary theme
            ]
    in
    button (css styles :: attributes) content


primaryLoader : Theme -> ActionResult a -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
primaryLoader theme actionResult attributes content =
    let
        visibleContent =
            if ActionResult.isLoading actionResult then
                [ fa "fa fa-spinner fa-spin" ]

            else
                content
    in
    primary theme ([ Attributes.disabled (ActionResult.isLoading actionResult) ] ++ attributes) visibleContent


colorful : Color -> Color -> Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
colorful color backgroundColor theme attributes content =
    let
        styles =
            [ Typography.heading3color color
            , buttonSharedStyle
            , buttonFilledStyle backgroundColor theme
            ]
    in
    button (css styles :: attributes) content


primaryLink : Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
primaryLink theme attributes content =
    let
        styles =
            [ Typography.heading3inversed theme
            , buttonSharedStyle
            , buttonFilledStyle theme.colors.primary theme
            , textDecoration none
            ]
    in
    a (css styles :: Attributes.class "button" :: attributes) content


actionResultPrimary : ActionResult a -> Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
actionResultPrimary actionResult theme attributes content =
    let
        ( newAttributes, newContent ) =
            if ActionResult.isLoading actionResult then
                ( [ Attributes.disabled True
                  , css [ important (minWidth (px2rem 120)) ]
                  ]
                    ++ attributes
                , [ fa "fas fa-spinner fa-spin" ]
                )

            else
                ( attributes, content )
    in
    primary theme newAttributes newContent


link : Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link theme attributes content =
    let
        colorHover =
            colorL10 theme.colors.primary

        style =
            [ Typography.copy1link theme
            , padding2 (px2rem Spacing.sm) zero
            , backgroundColor transparent
            , buttonSharedStyle
            , hover
                [ color colorHover ]
            ]
    in
    button (css style :: attributes) content


inline : Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
inline theme attributes content =
    let
        colorHover =
            colorL10 theme.colors.primary

        style =
            [ Typography.copy1link theme
            , padding zero
            , backgroundColor transparent
            , buttonSharedStyle
            , hover
                [ color colorHover ]
            ]
    in
    button (css style :: attributes) content


buttonSharedStyle : Style
buttonSharedStyle =
    Css.batch
        [ border zero
        , outline none
        , cursor pointer
        , display inlineFlex
        , alignItems center
        , descendants
            [ typeSelector "span"
                [ adjacentSiblings
                    [ class "fa"
                        [ marginLeft (px2rem Spacing.sm)
                        ]
                    ]
                ]
            , class "fa"
                [ adjacentSiblings
                    [ typeSelector "span"
                        [ marginLeft (px2rem Spacing.sm)
                        ]
                    ]
                ]
            ]
        ]


buttonFilledStyle : Color -> Theme -> Style
buttonFilledStyle color theme =
    let
        buttonColorHover =
            colorL10 color

        buttonColorActive =
            colorD05 color

        buttonColorDisabled =
            colorL20 color

        shadowColor =
            colorL40 color
    in
    Css.batch
        [ Border.roundedFull
        , Spacing.insetSquishMD
        , justifyContent center
        , backgroundColor color
        , transition
            [ Transition.default Css.Transitions.boxShadow3
            , Transition.default Css.Transitions.transform3
            , Transition.default Css.Transitions.backgroundColor3
            ]
        , hover
            [ Shadow.sm (\_ -> shadowColor) ()
            , transform (translateY (px -1))
            , backgroundColor buttonColorHover
            ]
        , active
            [ Shadow.sm (\_ -> shadowColor) ()
            , backgroundColor buttonColorActive
            , transform (translateY zero)
            ]
        , disabled
            [ backgroundColor buttonColorDisabled
            , cursor notAllowed
            , transform none
            , boxShadow none
            ]
        ]
