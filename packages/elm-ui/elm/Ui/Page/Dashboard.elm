module Ui.Page.Dashboard exposing
    ( Dashboard
    , dashboard
    , toHtml
    , withDescription
    , withNotices
    )

{-| The dashboard page: a title, and widgets in a grid.

The grid is two columns when the page is wide and one when it is not — decided
by the width of the page region, not of the window, because the navigation
beside it takes a different share of the same window collapsed and expanded.
DOM order is the order the widgets are given in, and it is the reading order in
both layouts: the grid fills row by row and nothing is placed out of turn, so
the keyboard walks the dashboard in the order the eye does.

The title is the page's `h1`, carrying `Ui.AppShell.mainHeadingId`, so the
focus that moves on a route change lands on it. Each widget's own heading is
an `h2` under it.

-}

import Html exposing (Html, div, h1, header, p, section, text)
import Html.Attributes exposing (class, id, tabindex)
import Ui.AppShell as AppShell
import Ui.Layout as Layout
import Ui.Widget as Widget exposing (Widget)


type Dashboard msg
    = Dashboard
        { title : String
        , description : Maybe String
        , widgets : List (Widget msg)
        , notices : List (Html msg)
        }


dashboard : { title : String, widgets : List (Widget msg) } -> Dashboard msg
dashboard config =
    Dashboard
        { title = config.title
        , description = Nothing
        , widgets = config.widgets
        , notices = []
        }


withDescription : String -> Dashboard msg -> Dashboard msg
withDescription description (Dashboard config) =
    Dashboard { config | description = Just description }


{-| Things to read before the widgets: announcements, a warning about the
instance. Typically `Ui.Alert`s, in the order given.

Between the header and the grid, so they come after the `h1` in reading order —
the page is still named first — and before anything the widgets hold. They are
not a widget: a notice is about the whole application, and in the grid it would
be read as one more card among the rest and could land in the second column.

-}
withNotices : List (Html msg) -> Dashboard msg -> Dashboard msg
withNotices notices (Dashboard config) =
    Dashboard { config | notices = notices }


toHtml : Dashboard msg -> Html msg
toHtml (Dashboard config) =
    -- The same frame as `Ui.Page.Content`: the outer element scrolls and the
    -- inner one is the measure.
    section [ class "ui-page" ]
        [ div [ class "ui-page__inner" ]
            [ header [ class "ui-page__header" ]
                [ div [ class "ui-page__heading" ]
                    [ -- tabindex -1 so the route-change focus has somewhere to
                      -- land; without it `Dom.focus` fails silently.
                      h1 [ class "ui-page-title", id AppShell.mainHeadingId, tabindex -1 ] [ text config.title ]
                    , case config.description of
                        Just description ->
                            p [ class "ui-body-sm ui-text--muted" ] [ text description ]

                        Nothing ->
                            text ""
                    ]
                ]
            , if List.isEmpty config.notices then
                text ""

              else
                Layout.stack config.notices
            , div [ class "ui-dashboard" ]
                (List.indexedMap
                    (\index entry -> Widget.toHtml ("ui-widget-" ++ String.fromInt (index + 1)) entry)
                    config.widgets
                )
            ]
        ]
