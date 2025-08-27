module Shared.Utils.TimeUtils exposing
    ( monthToInt
    , monthToString
    , toReadableDate
    , toReadableDateTime
    , toReadableTime
    )

import Gettext exposing (gettext)
import Time exposing (Month(..))


toReadableDateTime : Time.Zone -> Time.Posix -> String
toReadableDateTime timeZone time =
    let
        hour =
            String.fromInt <| Time.toHour timeZone time

        min =
            String.padLeft 2 '0' <| String.fromInt <| Time.toMinute timeZone time

        day =
            String.fromInt <| Time.toDay timeZone time

        month =
            String.fromInt <| monthToInt <| Time.toMonth timeZone time

        year =
            String.fromInt <| Time.toYear timeZone time
    in
    day ++ ". " ++ month ++ ". " ++ year ++ ", " ++ hour ++ ":" ++ min


toReadableDate : Time.Zone -> Time.Posix -> String
toReadableDate timeZone time =
    let
        day =
            String.fromInt <| Time.toDay timeZone time

        month =
            String.fromInt <| monthToInt <| Time.toMonth timeZone time

        year =
            String.fromInt <| Time.toYear timeZone time
    in
    day ++ ". " ++ month ++ ". " ++ year


toReadableTime : Time.Zone -> Time.Posix -> String
toReadableTime timeZone time =
    let
        hour =
            String.fromInt <| Time.toHour timeZone time

        min =
            String.padLeft 2 '0' <| String.fromInt <| Time.toMinute timeZone time
    in
    hour ++ ":" ++ min


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthToString : { a | locale : Gettext.Locale } -> Month -> String
monthToString appState month =
    case month of
        Jan ->
            gettext "January" appState.locale

        Feb ->
            gettext "February" appState.locale

        Mar ->
            gettext "March" appState.locale

        Apr ->
            gettext "April" appState.locale

        May ->
            gettext "May" appState.locale

        Jun ->
            gettext "June" appState.locale

        Jul ->
            gettext "July" appState.locale

        Aug ->
            gettext "August" appState.locale

        Sep ->
            gettext "September" appState.locale

        Oct ->
            gettext "October" appState.locale

        Nov ->
            gettext "November" appState.locale

        Dec ->
            gettext "December" appState.locale
