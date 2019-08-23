module Time.Distance exposing (calculateDistance, inWordsWithConfig, moreThanTwoMonths, upToOneDay, upToOneMinute, upToOneMonth, upToOneYear, upToTwoMonths)

{-| Get the string representing the time interval, defaulting to English, with
affixes ("ago"/"in").

The second argument is for the "now" time.

-}

import Time
import Time.Distance.Types exposing (Config, DistanceId(..), Locale, Tense(..))


minutes_in_day : number
minutes_in_day =
    1440


minutes_in_almost_two_days : number
minutes_in_almost_two_days =
    2520


minutes_in_month : number
minutes_in_month =
    43200


minutes_in_two_months : number
minutes_in_two_months =
    86400


{-| Get the string representing the time interval, with another language, and/
or without affixes ("ago"/"in").

`Config` is a type alias for `{ withAffix : Bool}`.

-}
inWordsWithConfig :
    Config
    -> Locale
    -> Time.Posix
    -> Time.Posix
    -> String
inWordsWithConfig config locale otherTime currentTime =
    let
        currentSeconds =
            currentTime
                |> Time.posixToMillis

        otherSeconds =
            otherTime
                |> Time.posixToMillis

        tense =
            case compare currentSeconds otherSeconds of
                GT ->
                    Past

                _ ->
                    Future

        distance =
            calculateDistance True otherSeconds currentSeconds
    in
    locale config tense distance


upToOneMinute : Int -> DistanceId
upToOneMinute seconds =
    if seconds < 5 then
        LessThanXSeconds 5

    else if seconds < 10 then
        LessThanXSeconds 10

    else if seconds < 20 then
        LessThanXSeconds 20

    else if seconds < 40 then
        HalfAMinute

    else if seconds < 60 then
        LessThanXMinutes 1

    else
        XMinutes 1


upToOneDay : Int -> DistanceId
upToOneDay minutes =
    let
        hours =
            round <| toFloat minutes / 60
    in
    AboutXHours hours


upToOneMonth : Int -> DistanceId
upToOneMonth minutes =
    let
        days =
            round <| toFloat minutes / minutes_in_day
    in
    XDays days


upToTwoMonths : Int -> DistanceId
upToTwoMonths minutes =
    let
        months =
            round <| toFloat minutes / minutes_in_month
    in
    AboutXMonths months


upToOneYear : Int -> DistanceId
upToOneYear minutes =
    let
        nearestMonth =
            round <| toFloat minutes / minutes_in_month
    in
    XMonths nearestMonth


moreThanTwoMonths : Int -> DistanceId
moreThanTwoMonths minutes =
    let
        months =
            minutes // minutes_in_month
    in
    if months < 12 then
        -- 2 months up to 12 months
        upToOneYear minutes

    else
        -- 1 year up to max Date
        let
            monthsSinceStartOfYear =
                modBy 12 months

            years =
                floor <| toFloat months / 12
        in
        if monthsSinceStartOfYear < 3 then
            -- N years up to 1 years 3 months
            AboutXYears years

        else if monthsSinceStartOfYear < 9 then
            -- N years 3 months up to N years 9 months
            OverXYears years

        else
            -- N years 9 months up to N year 12 months
            AlmostXYears <| years + 1


calculateDistance :
    Bool
    -> Int
    -> Int
    -> DistanceId
calculateDistance includeSeconds s1 s2 =
    let
        seconds =
            abs (s2 - s1) // 1000

        minutes =
            round <| toFloat seconds / 60
    in
    if includeSeconds && minutes < 2 then
        upToOneMinute seconds

    else if minutes == 0 then
        LessThanXMinutes 1

    else if minutes < 2 then
        XMinutes minutes

    else if minutes < 45 then
        -- 2 mins up to 0.75 hrs
        XMinutes minutes

    else if minutes < 90 then
        -- 0.75 hrs up to 1.5 hrs
        AboutXHours 1

    else if minutes < minutes_in_day then
        -- 1.5 hrs up to 24 hrs
        upToOneDay minutes

    else if minutes < minutes_in_almost_two_days then
        -- 1 day up to 1.75 days
        XDays 1

    else if minutes < minutes_in_month then
        -- 1.75 days up to 30 days
        upToOneMonth minutes

    else if minutes < minutes_in_two_months then
        -- 1 month up to 2 months
        upToTwoMonths minutes

    else
        moreThanTwoMonths minutes
