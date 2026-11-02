module Ui.Chart exposing
    ( Chart
    , Series
    , bar
    , doughnut
    , line
    , radar
    , series
    , toHtml
    , withAxisTitles
    , withHiddenCaption
    , withLegend
    , withRange
    , withSummary
    )

{-| A chart, and the same numbers in a form that can be read out.

The existing wrapper draws a `canvas` with no role, no name and nothing behind
it. To a screen reader that is an empty element; to a keyboard it is a picture
whose values can only be reached by hovering them, which is a pointer gesture
and therefore not a way to reach information at all (1.1.1, 2.1.1).

So this draws two things from one set of numbers:

  - **The picture**, as an image with a name and a one-line summary. That is all
    a chart can honestly be to a reader who cannot see it: a shape, described.
  - **The numbers**, as a table. Hidden from the screen, not from anything that
    reads the page. Generated from the same data as the chart, so the
    alternative cannot drift from what is drawn — which is the failure mode of
    every hand-written one.

**Keyboard.** There is deliberately none: no focusable points, no arrow-key
walk through the series. Chart.js has no accessible interaction to expose, and
building one would mean inventing a widget with no established pattern, when the
information it would give is already in the table. What 2.1.1 asks is that the
_information_ be reachable without a pointer, not that the picture be walkable.

-}

import Html exposing (Html, caption, figcaption, figure, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, scope)
import Json.Encode as Encode
import List.Extra as List


type Chart
    = Chart
        { kind : String
        , label : String
        , summary : Maybe String
        , categories : List String
        , series : List Series
        , legend : Bool
        , axisTitles : Maybe { x : String, y : String }
        , range : Maybe { min : Float, max : Float }
        , captionHidden : Bool
        }


type Series
    = Series { label : String, values : List Float }


{-| One line, bar or ring of numbers. Named, because the legend and the table
both need to call it something — and because a series with no name is a color,
which is not a way to tell two of them apart (1.4.1).
-}
series : { label : String, values : List Float } -> Series
series =
    Series


{-| `label` is what the chart is _of_ — "Projects created per month". It names
the image and captions the table.
-}
bar : { label : String, categories : List String, series : List Series } -> Chart
bar config =
    build "bar" config


line : { label : String, categories : List String, series : List Series } -> Chart
line config =
    build "line" config


{-| One series, drawn as slices. The categories are the slices rather than an
axis, which is why it takes the same shape and reads differently.
-}
doughnut : { label : String, categories : List String, series : List Series } -> Chart
doughnut config =
    build "doughnut" config


{-| The categories as spokes round a center, each series a closed shape across
them — several scores of one thing at a glance, a project's metrics say. Needs
three categories to be a shape at all; with fewer, a table says it better.

**Colored by the brand.** A radar usually draws one series, and that one takes
the theme's primary color — its line and points in `--ui-brand-strong`, which
clears 3:1 on any ground for any seed, and a translucent fill in `--ui-brand` —
read when the chart is drawn, so a tenant's color shows. A second series and
after take the chart palette, so no two share a color.

Give it `withRange` when the values have a known scale; otherwise the spokes
start at zero and end at the largest value. With a range the rings are its
quarters — 0.25, 0.5, 0.75 and 1 for a measure — and they carry no numbers: the
exact values are in the data table the chart renders for assistive technology.

-}
radar : { label : String, categories : List String, series : List Series } -> Chart
radar config =
    build "radar" config


build : String -> { label : String, categories : List String, series : List Series } -> Chart
build kind config =
    Chart
        { kind = kind
        , label = config.label
        , summary = Nothing
        , categories = config.categories
        , series = config.series
        , legend = True
        , axisTitles = Nothing
        , range = Nothing
        , captionHidden = False
        }


{-| A sentence saying what the picture shows — "rising from 4 in January to 21
in June". It is the part a reader who cannot see the chart gets instead of its
shape, and no component can write it: only the page knows what the numbers mean.
-}
withSummary : String -> Chart -> Chart
withSummary summary (Chart config) =
    Chart { config | summary = Just summary }


withLegend : Bool -> Chart -> Chart
withLegend legend (Chart config) =
    Chart { config | legend = legend }


withAxisTitles : { x : String, y : String } -> Chart -> Chart
withAxisTitles titles (Chart config) =
    Chart { config | axisTitles = Just titles }


{-| The label kept for everything that reads the page but taken off the
screen — for a chart whose subject is already written above it, where a caption
would say the heading a second time.

The chart keeps its name either way: the picture is named by its `aria-label`
and the numbers by their table's own caption, so hiding this only removes the
line a sighted reader would have read twice.

-}
withHiddenCaption : Chart -> Chart
withHiddenCaption (Chart config) =
    Chart { config | captionHidden = True }


{-| Fix the value axis — the y axis of a bar or line chart, the spokes of a
radar — to a known scale, rather than letting it end at the largest value. A
score from 0 to 1 is `withRange { min = 0, max = 1 }`, so a project scoring 0.4
everywhere does not fill the picture. Ignored by a doughnut, which has no axis.
-}
withRange : { min : Float, max : Float } -> Chart -> Chart
withRange range (Chart config) =
    Chart { config | range = Just range }


toHtml : Chart -> Html msg
toHtml (Chart config) =
    figure [ classList [ ( "ui-chart", True ), ( "ui-chart--radar", config.kind == "radar" ) ] ]
        (Html.node "ui-chart"
            ([ class "ui-chart__canvas"
             , attribute "chart-type" config.kind

             -- The picture is an image with a name. Its children are
             -- presentational because of the role, which is what keeps
             -- Chart.js's canvas fallback from being read as the whole chart.
             , attribute "role" "img"
             , attribute "aria-label" (name config)
             , attribute "legend"
                (if config.legend then
                    "true"

                 else
                    "false"
                )
             , Html.Attributes.property "chartData" (encode config)
             ]
                ++ (case config.axisTitles of
                        Just titles ->
                            [ attribute "x-title" titles.x, attribute "y-title" titles.y ]

                        Nothing ->
                            []
                   )
            )
            []
            :: viewCaption config
            ++ [ viewTable config ]
        )


name : { a | label : String, summary : Maybe String } -> String
name config =
    case config.summary of
        Just summary ->
            config.label ++ ". " ++ summary

        Nothing ->
            config.label


viewCaption : { a | label : String, captionHidden : Bool } -> List (Html msg)
viewCaption config =
    [ figcaption
        [ classList
            [ ( "ui-chart__caption", True )
            , ( "ui-visually-hidden", config.captionHidden )
            ]
        ]
        [ text config.label ]
    ]


{-| The numbers, off the screen and in the accessibility tree.

A real table, so it is navigated as one: the categories are the row headers and
the series are the columns, which is the shape every chart here reduces to — a
radar's spokes are its rows, as a bar chart's bars are.

-}
viewTable : { a | label : String, categories : List String, series : List Series } -> Html msg
viewTable config =
    table [ class "ui-chart__data ui-visually-hidden" ]
        [ caption [] [ text config.label ]
        , thead []
            [ tr []
                -- The corner heads nothing: the row headers name the
                -- categories and the other columns the series. A cell, not an
                -- empty header, which would be announced as a column with no
                -- name.
                (td [] []
                    :: List.map (\(Series entry) -> th [ scope "col" ] [ text entry.label ]) config.series
                )
            ]
        , tbody []
            (List.indexedMap
                (\index category ->
                    tr []
                        (th [ scope "row" ] [ text category ]
                            :: List.map (\entry -> td [] [ text (valueAt index entry) ]) config.series
                        )
                )
                config.categories
            )
        ]


valueAt : Int -> Series -> String
valueAt index (Series entry) =
    entry.values
        |> List.getAt index
        |> Maybe.map String.fromFloat
        |> Maybe.withDefault ""


encode : { a | categories : List String, series : List Series, range : Maybe { min : Float, max : Float } } -> Encode.Value
encode config =
    Encode.object
        [ ( "categories", Encode.list Encode.string config.categories )
        , ( "range"
          , case config.range of
                Just range ->
                    Encode.object [ ( "min", Encode.float range.min ), ( "max", Encode.float range.max ) ]

                Nothing ->
                    Encode.null
          )
        , ( "series"
          , Encode.list
                (\(Series entry) ->
                    Encode.object
                        [ ( "label", Encode.string entry.label )
                        , ( "values", Encode.list Encode.float entry.values )
                        ]
                )
                config.series
          )
        ]
