module Ui.Pagination exposing
    ( Words
    , toHtml
    , toHtmlWithWords
    )

{-| Page links for a listing.

The shape is the one the existing listing has: first and previous, a window of
numbers around the current page with an ellipsis where it has been cut, then
next and last. First and last appear only when they would go somewhere;
previous and next are always there and are disabled at the ends, so the row does
not change width as you move through it.

`aria-current="page"` marks the page you are on — the only thing that says which
number is live, since a highlight is not something a screen reader reads.
Disabled ends are `disabled` rather than styled to look it, so Tab skips them
instead of landing on a button that does nothing.

-}

import Html exposing (Html, button, li, nav, ol, span, text)
import Html.Attributes exposing (attribute, class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Ui.Icon as Icon


{-| How far either side of the current page the numbers run before they are cut
short. Four, as the existing listing does — nine numbers is enough to jump
around in and few enough to read.
-}
window : Int
window =
    4


toHtml : { current : Int, total : Int, onSelect : Int -> msg } -> Html msg
toHtml =
    toHtmlWithWords english


{-| What the pagination says: the name of its landmark and the four steps.
`toHtml` says them in English; pass the page's language here.
-}
type alias Words =
    { pagination : String
    , first : String
    , previous : String
    , next : String
    , last : String
    }


english : Words
english =
    { pagination = "Pagination"
    , first = "First"
    , previous = "Prev"
    , next = "Next"
    , last = "Last"
    }


{-| `toHtml`, with the words given.
-}
toHtmlWithWords : Words -> { current : Int, total : Int, onSelect : Int -> msg } -> Html msg
toHtmlWithWords words config =
    if config.total <= 1 then
        -- One page is not a set of pages.
        text ""

    else
        let
            from =
                max 1 (config.current - window)

            to =
                min config.total (config.current + window)
        in
        nav [ class "ui-pagination", attribute "aria-label" words.pagination ]
            [ ol [ class "ui-pagination__list" ]
                (List.concat
                    [ edge (config.current > 1)
                        { label = words.first, icon = "fa-angles-left", leading = True, page = 1 }
                        config
                    , [ step
                            { label = words.previous
                            , icon = "fa-angle-left"
                            , leading = True
                            , page = config.current - 1
                            , enabled = config.current > 1
                            }
                            config
                      ]
                    , ellipsis (from > 1)
                    , List.map (number config) (List.range from to)
                    , ellipsis (to < config.total)
                    , [ step
                            { label = words.next
                            , icon = "fa-angle-right"
                            , leading = False
                            , page = config.current + 1
                            , enabled = config.current < config.total
                            }
                            config
                      ]
                    , edge (config.current < config.total)
                        { label = words.last, icon = "fa-angles-right", leading = False, page = config.total }
                        config
                    ]
                )
            ]


number : { a | current : Int, onSelect : Int -> msg } -> Int -> Html msg
number config page =
    li [ class "ui-pagination__item" ]
        [ button
            ([ type_ "button"
             , classList
                [ ( "ui-pagination__link", True )
                , ( "is-current", page == config.current )
                ]
             , onClick (config.onSelect page)
             ]
                ++ (if page == config.current then
                        [ attribute "aria-current" "page" ]

                    else
                        []
                   )
            )
            [ text (String.fromInt page) ]
        ]


{-| First and last, which only appear when they lead somewhere.
-}
edge :
    Bool
    -> { label : String, icon : String, leading : Bool, page : Int }
    -> { a | onSelect : Int -> msg }
    -> List (Html msg)
edge visible link config =
    if visible then
        [ step
            { label = link.label
            , icon = link.icon
            , leading = link.leading
            , page = link.page
            , enabled = True
            }
            config
        ]

    else
        []


step :
    { label : String, icon : String, leading : Bool, page : Int, enabled : Bool }
    -> { a | onSelect : Int -> msg }
    -> Html msg
step link config =
    let
        glyph =
            span [ class "ui-pagination__icon" ] [ Icon.toHtml (Icon.solid link.icon) ]

        label =
            text link.label
    in
    li [ class "ui-pagination__item" ]
        [ button
            [ type_ "button"
            , class "ui-pagination__link"
            , disabled (not link.enabled)
            , onClick (config.onSelect link.page)
            ]
            (if link.leading then
                [ glyph, label ]

             else
                [ label, glyph ]
            )
        ]


{-| The gap where the numbers were cut. Presentational: it is a gap, not a
destination, so nothing lands on it and nothing announces it.
-}
ellipsis : Bool -> List (Html msg)
ellipsis visible =
    if visible then
        [ li [ class "ui-pagination__item", attribute "aria-hidden" "true" ]
            [ span [ class "ui-pagination__ellipsis" ] [ text "…" ] ]
        ]

    else
        []
