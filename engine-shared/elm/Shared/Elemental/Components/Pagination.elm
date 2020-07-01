module Shared.Elemental.Components.Pagination exposing
    ( PaginationConfig
    , view
    )

import Css exposing (alignItems, backgroundColor, center, display, displayFlex, hover, important, inlineBlock, justifyContent, margin, marginLeft, marginRight, minWidth, none, padding2, textAlign, textDecoration, zero)
import Css.Global as Css exposing (descendants, typeSelector)
import Html.Styled exposing (Html, a, li, nav, span, text, ul)
import Html.Styled.Attributes exposing (class, classList, css, href)
import Shared.Data.Pagination.Page exposing (Page)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Html.Styled exposing (emptyNode, fa)


type alias PaginationConfig =
    { paginationQueryString : PaginationQueryString
    , page : Page
    , toRoute : PaginationQueryString -> String
    }


view : Theme -> PaginationConfig -> Html msg
view theme cfg =
    let
        paginationQueryString =
            cfg.paginationQueryString

        currentPage =
            Maybe.withDefault 1 cfg.paginationQueryString.page

        viewPageLink pageNumber attributes content =
            a (attributes ++ [ href <| cfg.toRoute { paginationQueryString | page = Just pageNumber } ])
                content

        viewNavLink number =
            li [ classList [ ( "active", number == currentPage ) ] ]
                [ viewPageLink number
                    []
                    [ text (String.fromInt number) ]
                ]

        firstLink =
            if currentPage > 1 then
                viewPageLink 1
                    [ class "nav-link icon-left" ]
                    [ fa "fas fa-angle-double-left"
                    , text "First"
                    ]

            else
                emptyNode

        prevLink =
            if currentPage > 1 then
                viewPageLink (currentPage - 1)
                    [ class "nav-link icon-left" ]
                    [ fa "fas fa-angle-left"
                    , text "Prev"
                    ]

            else
                emptyNode

        dots =
            li [] [ span [ class "dots" ] [ text "..." ] ]

        ( left, leftDots ) =
            if currentPage - 4 > 1 then
                ( currentPage - 4, dots )

            else
                ( 1, emptyNode )

        ( right, rightDots ) =
            if currentPage + 4 < cfg.page.totalPages then
                ( currentPage + 4, dots )

            else
                ( cfg.page.totalPages, emptyNode )

        pageLinks =
            List.map viewNavLink (List.range left right)

        nextLink =
            if currentPage < cfg.page.totalPages then
                viewPageLink (currentPage + 1)
                    [ class "nav-link icon-right" ]
                    [ text "Next"
                    , fa "fas fa-angle-right"
                    ]

            else
                emptyNode

        lastLink =
            if currentPage < cfg.page.totalPages then
                viewPageLink cfg.page.totalPages
                    [ class "nav-link icon-right" ]
                    [ text "Last"
                    , fa "fas fa-angle-double-right"
                    ]

            else
                emptyNode

        links =
            ul [ class "pagination" ] ([ leftDots ] ++ pageLinks ++ [ rightDots ])

        styles =
            [ Spacing.stackXL
            , displayFlex
            , justifyContent center
            , alignItems center
            , descendants
                [ Css.class "pagination"
                    [ displayFlex
                    , backgroundColor theme.colors.backgroundTint
                    , Border.roundedFull
                    , important (margin zero)
                    , padding2 zero (px2rem Spacing.sm)
                    , descendants
                        [ typeSelector "li"
                            [ important (margin zero)
                            , descendants
                                [ typeSelector "a"
                                    [ Typography.copy1 theme
                                    , Spacing.insetSM
                                    , display inlineBlock
                                    , textDecoration none
                                    , minWidth (px2rem 40)
                                    , textAlign center
                                    , hover
                                        [ Typography.copy1link theme
                                        ]
                                    ]
                                , Css.class "dots"
                                    [ Spacing.insetSM
                                    , display inlineBlock
                                    ]
                                ]
                            ]
                        , Css.class "active"
                            [ descendants
                                [ typeSelector "a"
                                    [ Typography.copy1inversed theme
                                    , backgroundColor theme.colors.primary
                                    , Border.roundedDefault
                                    , hover
                                        [ Typography.copy1inversed theme
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , Css.class "nav-link"
                    [ Typography.copy1 theme
                    , textDecoration none
                    , hover [ Typography.copy1link theme ]
                    ]
                , Css.class "icon-left"
                    [ marginRight (px2rem Spacing.md)
                    , descendants
                        [ Css.class "fa" [ marginRight (px2rem Spacing.xs) ]
                        ]
                    ]
                , Css.class "icon-right"
                    [ marginLeft (px2rem Spacing.md)
                    , descendants
                        [ Css.class "fa" [ marginLeft (px2rem Spacing.xs) ]
                        ]
                    ]
                ]
            ]
    in
    if cfg.page.totalPages > 1 then
        nav [ css styles ] [ firstLink, prevLink, links, nextLink, lastLink ]

    else
        emptyNode
