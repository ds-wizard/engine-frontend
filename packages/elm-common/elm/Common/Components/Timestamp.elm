module Common.Components.Timestamp exposing
    ( created
    , listItem
    , updated
    , view
    , viewUpdated
    )

{-| A moment in time, said three ways: how long ago, exactly when, and in a
form a machine can read.

Relative time is what people scan a list for ("3 days ago"), but it is an
approximation, and the exact time is what somebody reaching for it actually
wants. The existing listing put the exact time only in a hover tooltip, which
nothing but a mouse can reach. Here every shape carries all three:

  - `listItem` and `updated` produce the record `Ui.ListItem.withTimestamp`
    takes, which renders a `time` element with the ISO value in `datetime`.
  - `view` renders an inline `time` element for use in running text, where the
    exact time is part of the element's text for a screen reader (visually
    hidden) and a `title` for a pointer — so the tooltip is never the only place
    it lives. `time` is not focusable and should not be made so just to show a
    tooltip: a tab stop that does nothing is a cost every keyboard user pays.
    `viewUpdated` is the same with the "Updated 3 days ago" wording, for a row
    that keeps its modification time under its title as a fragment.

The relative wording comes from `gingko/time-distance` with the application's
translations (`Common.Utils.TimeDistance`), the absolute from
`Common.Utils.TimeUtils.toReadableDateTime` in the user's zone, and the machine
value is ISO 8601 in UTC.

-}

import Common.Data.Context exposing (WithTime)
import Common.Utils.TimeDistance as TimeDistance
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html, text, time)
import Html.Attributes exposing (attribute, title)
import Iso8601
import String.Format as String
import Time
import Time.Distance
import Ui.Text as Text


{-| "3 days ago", with the exact and the machine-readable time.
-}
listItem : WithTime a -> Time.Posix -> { relative : String, absolute : String, machine : String }
listItem appState moment =
    { relative = relative appState moment
    , absolute = TimeUtils.toReadableDateTime appState.timeZone moment
    , machine = Iso8601.fromTime moment
    }


{-| "Created 3 days ago" — for a listing whose items do not change after they
are made, where a bare time says nothing about what happened then.
-}
created : WithTime a -> Time.Posix -> { relative : String, absolute : String, machine : String }
created appState moment =
    let
        stamp =
            listItem appState moment
    in
    { stamp | relative = String.format (gettext "Created %s" appState.locale) [ stamp.relative ] }


{-| "Updated 3 days ago" — the wording every listing uses for its items'
modification time, with a msgid the translations already have.
-}
updated : WithTime a -> Time.Posix -> { relative : String, absolute : String, machine : String }
updated appState moment =
    let
        stamp =
            listItem appState moment
    in
    { stamp | relative = String.format (gettext "Updated %s" appState.locale) [ stamp.relative ] }


{-| An inline `time` element: the relative time on screen, the exact time in a
`title` for a pointer and in visually hidden text for everyone else.
-}
view : WithTime a -> Time.Posix -> Html msg
view appState moment =
    viewStamp (listItem appState moment)


{-| "Updated 3 days ago" as an inline `time` element, like `view`.
-}
viewUpdated : WithTime a -> Time.Posix -> Html msg
viewUpdated appState moment =
    viewStamp (updated appState moment)


viewStamp : { relative : String, absolute : String, machine : String } -> Html msg
viewStamp stamp =
    time
        [ attribute "datetime" stamp.machine
        , title stamp.absolute
        ]
        [ text stamp.relative

        -- Inline: a `time` element holds phrasing content only.
        , Text.visuallyHiddenInline (" (" ++ stamp.absolute ++ ")")
        ]


relative : WithTime a -> Time.Posix -> String
relative appState moment =
    Time.Distance.inWordsWithConfig { withAffix = True }
        (TimeDistance.locale appState.locale)
        moment
        appState.currentTime
