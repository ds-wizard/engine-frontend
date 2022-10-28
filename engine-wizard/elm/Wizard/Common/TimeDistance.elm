module Wizard.Common.TimeDistance exposing (locale)

import Gettext exposing (gettext, ngettext)
import String.Format as String
import Time.Distance.Types exposing (DistanceId(..), Locale, Tense(..))
import Wizard.Common.AppState exposing (AppState)


locale : AppState -> Locale
locale appState { withAffix } tense distanceId =
    let
        toStr =
            String.fromInt

        maybeAffix str =
            case ( withAffix, tense ) of
                ( True, Past ) ->
                    String.format (gettext "%s ago" appState.locale) [ str ]

                ( True, Future ) ->
                    String.format (gettext "in %s" appState.locale) [ str ]

                ( False, _ ) ->
                    str

        lessThanMinutes n =
            String.format (ngettext ( "less than a minute", "less than %s minutes" ) n appState.locale) [ toStr n ]
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
            String.format (ngettext ( "1 minute", "%s minutes" ) n appState.locale) [ toStr n ]

        AboutXHours n ->
            String.format (ngettext ( "about 1 hour", "about %s hours" ) n appState.locale) [ toStr n ]

        XDays n ->
            String.format (ngettext ( "1 day", "%s days" ) n appState.locale) [ toStr n ]

        AboutXMonths n ->
            String.format (ngettext ( "about 1 month", "about %s months" ) n appState.locale) [ toStr n ]

        XMonths n ->
            String.format (ngettext ( "1 month", "%s months" ) n appState.locale) [ toStr n ]

        AboutXYears n ->
            String.format (ngettext ( "about 1 year", "about %s years" ) n appState.locale) [ toStr n ]

        OverXYears n ->
            String.format (ngettext ( "over 1 year", "over %s years" ) n appState.locale) [ toStr n ]

        AlmostXYears n ->
            String.format (ngettext ( "almost 1 year", "almost %s years" ) n appState.locale) [ toStr n ]
    )
        |> maybeAffix
