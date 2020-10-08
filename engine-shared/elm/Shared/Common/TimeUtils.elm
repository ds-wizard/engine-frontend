module Shared.Common.TimeUtils exposing (toReadableDateTime)

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
    day ++ ". " ++ month ++ ". " ++ year ++ ", " ++ hour ++ ":" ++ min ++ ""


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
