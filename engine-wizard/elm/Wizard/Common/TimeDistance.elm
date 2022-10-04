module Wizard.Common.TimeDistance exposing (locale)

import Shared.Locale exposing (l, lf)
import Time.Distance.Types exposing (DistanceId(..), Locale, Tense(..))
import Wizard.Common.AppState exposing (AppState)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.TimeDistance"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.TimeDistance"


locale : AppState -> Locale
locale appState { withAffix } tense distanceId =
    let
        toStr =
            String.fromInt

        maybeAffix str =
            case ( withAffix, tense ) of
                ( True, Past ) ->
                    lf_ "ago" [ str ] appState

                ( True, Future ) ->
                    lf_ "in" [ str ] appState

                ( False, _ ) ->
                    str
    in
    (case distanceId of
        -- We don't need that much granularity here
        LessThanXSeconds _ ->
            l_ "lessThan1Minute" appState

        HalfAMinute ->
            l_ "lessThan1Minute" appState

        LessThanXMinutes i ->
            if i == 1 then
                l_ "lessThan1Minute" appState

            else
                lf_ "lessThanXMinute" [ toStr i ] appState

        XMinutes i ->
            if i == 1 then
                l_ "1Minute" appState

            else
                lf_ "xMinutes" [ toStr i ] appState

        AboutXHours i ->
            if i == 1 then
                l_ "about1Hour" appState

            else
                lf_ "aboutXHours" [ toStr i ] appState

        XDays i ->
            if i == 1 then
                l_ "1Day" appState

            else
                lf_ "XDays" [ toStr i ] appState

        AboutXMonths i ->
            if i == 1 then
                l_ "about1Month" appState

            else
                lf_ "aboutXMonths" [ toStr i ] appState

        XMonths i ->
            if i == 1 then
                l_ "1Month" appState

            else
                lf_ "XMonths" [ toStr i ] appState

        AboutXYears i ->
            if i == 1 then
                l_ "about1Year" appState

            else
                lf_ "aboutXYears" [ toStr i ] appState

        OverXYears i ->
            if i == 1 then
                l_ "over1Year" appState

            else
                lf_ "overXYears" [ toStr i ] appState

        AlmostXYears i ->
            if i == 1 then
                l_ "almost1Year" appState

            else
                lf_ "almostXYears" [ toStr i ] appState
    )
        |> maybeAffix
