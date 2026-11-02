module Common.Components.UsageTable exposing
    ( Row
    , bytes
    , count
    , view
    )

{-| How much of each limit a tenant has used.

A `Ui.Table` with one row per resource: what it is, the figure, and how full it
is. The bar is `Ui.ProgressBar` with `towardsLimit`, so it is a `progressbar`
with its value on it rather than a colored box, and its tone — fine, near the
limit, over it — is the same rule in every usage table. The figure is written
out beside the bar as well, because a bar alone cannot be read to the byte and
its color cannot be the only sign that a limit is reached (1.4.1).

A resource with no limit has no bar: it says "Unlimited" instead, in words.

The rows are plain values, so any application's usage can be listed without this knowing their types.

-}

import Common.Api.Models.UsageValue exposing (UsageValue)
import Common.Components.UiWords as UiWords
import Common.Data.Context exposing (WithLocale)
import Common.Utils.ByteUnits as ByteUnits
import Gettext exposing (gettext)
import Html exposing (Html, text)
import Ui.Badge as Badge
import Ui.ProgressBar as ProgressBar
import Ui.Table as Table


type Row
    = Row
        { key : String
        , label : String
        , usage : UsageValue
        , format : Int -> String
        }


{-| A resource counted in units — users, projects. `key` names the row for its
test hook, `usage_row_<key>`: stable, where the label is translated.
-}
count : String -> String -> UsageValue -> Row
count key label usage =
    Row { key = key, label = label, usage = usage, format = String.fromInt }


{-| A resource measured in bytes — storage. Shown as "1.2 GB", not a raw number.
-}
bytes : String -> String -> UsageValue -> Row
bytes key label usage =
    Row { key = key, label = label, usage = usage, format = ByteUnits.toReadable }


{-| `showSoftLimits`: a resource without a hard limit may still have a soft one,
stored as a negative maximum. Administrators see it beside the figure; everyone
else sees "Unlimited".
-}
view : WithLocale a -> { showSoftLimits : Bool, rows : List Row } -> Html msg
view appState config =
    Table.table
        [ Table.column (gettext "Name" appState.locale) (\(Row row) -> text row.label)
        , Table.column (gettext "Usage" appState.locale) (viewFigure config.showSoftLimits)
        , Table.column (gettext "Limit" appState.locale) (viewLimit appState)
        ]
        config.rows
        |> Table.withRowTestId (\(Row row) -> "usage_row_" ++ row.key)
        |> Table.withWords (UiWords.table appState)
        |> Table.toHtml


viewFigure : Bool -> Row -> Html msg
viewFigure showSoftLimits (Row row) =
    if row.usage.max > 0 then
        text (row.format row.usage.current ++ " / " ++ row.format row.usage.max)

    else if showSoftLimits && row.usage.max < 0 then
        text (row.format row.usage.current ++ " / " ++ row.format -row.usage.max)

    else
        text (row.format row.usage.current)


viewLimit : WithLocale a -> Row -> Html msg
viewLimit appState (Row row) =
    if row.usage.max > 0 then
        let
            value =
                { value = toFloat row.usage.current, max = toFloat row.usage.max }
        in
        ProgressBar.toHtmlWithWords (UiWords.progressBar appState)
            { label = row.label, value = value.value, max = value.max, tone = ProgressBar.towardsLimit value }

    else
        Badge.toHtml (Badge.info (gettext "Unlimited" appState.locale))
