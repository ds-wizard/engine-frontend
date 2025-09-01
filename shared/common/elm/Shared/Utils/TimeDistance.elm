module Shared.Utils.TimeDistance exposing (locale)

import Gettext exposing (gettext, ngettext)
import String.Format as String
import Time.Distance.Types exposing (DistanceId(..), Locale, Tense(..))


locale : Gettext.Locale -> Locale
locale gettextLocale { withAffix } tense distanceId =
    let
        toStr =
            String.fromInt

        maybeAffix str =
            case ( withAffix, tense ) of
                ( True, Past ) ->
                    String.format (gettext "%s ago" gettextLocale) [ str ]

                ( True, Future ) ->
                    String.format (gettext "in %s" gettextLocale) [ str ]

                ( False, _ ) ->
                    str

        lessThanMinutes n =
            String.format (ngettext ( "less than a minute", "less than %s minutes" ) n gettextLocale) [ toStr n ]
    in
    (case distanceId of
        -- We don't need that much granularity here
        LessThanXSeconds _ ->
            lessThanMinutes 1

        HalfAMinute ->
            lessThanMinutes 1

        LessThanXMinutes i ->
            lessThanMinutes i

        XMinutes n ->
            String.format (ngettext ( "1 minute", "%s minutes" ) n gettextLocale) [ toStr n ]

        AboutXHours n ->
            String.format (ngettext ( "about 1 hour", "about %s hours" ) n gettextLocale) [ toStr n ]

        XDays n ->
            String.format (ngettext ( "1 day", "%s days" ) n gettextLocale) [ toStr n ]

        AboutXMonths n ->
            String.format (ngettext ( "about 1 month", "about %s months" ) n gettextLocale) [ toStr n ]

        XMonths n ->
            String.format (ngettext ( "1 month", "%s months" ) n gettextLocale) [ toStr n ]

        AboutXYears n ->
            String.format (ngettext ( "about 1 year", "about %s years" ) n gettextLocale) [ toStr n ]

        OverXYears n ->
            String.format (ngettext ( "over 1 year", "over %s years" ) n gettextLocale) [ toStr n ]

        AlmostXYears n ->
            String.format (ngettext ( "almost 1 year", "almost %s years" ) n gettextLocale) [ toStr n ]
    )
        |> maybeAffix
