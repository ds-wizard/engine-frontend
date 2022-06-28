module Shared.Common.TimeUtils exposing
    ( fromYMD
    , isAfter
    , isBefore
    , isBetween
    , monthToInt
    , monthToString
    , toReadableDate
    , toReadableDateTime
    , toReadableTime
    )

import Shared.Locale exposing (lg)
import Shared.Provisioning exposing (Provisioning)
import Time exposing (Month(..))
import Time.Extra as Time


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


fromYMD : Time.Zone -> Int -> Int -> Int -> Time.Posix
fromYMD timeZone year month day =
    Time.partsToPosix timeZone <|
        Time.Parts year (intToMonth month) day 0 0 0 0


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


intToMonth : Int -> Month
intToMonth month =
    case month of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


monthToString : { a | provisioning : Provisioning } -> Month -> String
monthToString appState month =
    case month of
        Jan ->
            lg "month.january" appState

        Feb ->
            lg "month.february" appState

        Mar ->
            lg "month.march" appState

        Apr ->
            lg "month.april" appState

        May ->
            lg "month.may" appState

        Jun ->
            lg "month.june" appState

        Jul ->
            lg "month.july" appState

        Aug ->
            lg "month.august" appState

        Sep ->
            lg "month.september" appState

        Oct ->
            lg "month.october" appState

        Nov ->
            lg "month.november" appState

        Dec ->
            lg "month.december" appState


isBetween : Time.Posix -> Time.Posix -> Time.Posix -> Bool
isBetween start end time =
    let
        startMillis =
            Time.posixToMillis start

        endMillis =
            Time.posixToMillis end

        timeMillis =
            Time.posixToMillis time
    in
    startMillis < timeMillis && endMillis > timeMillis


isBefore : Time.Posix -> Time.Posix -> Bool
isBefore expected time =
    let
        expectedMillis =
            Time.posixToMillis expected

        timeMillis =
            Time.posixToMillis time
    in
    timeMillis < expectedMillis


isAfter : Time.Posix -> Time.Posix -> Bool
isAfter expected time =
    let
        expectedMillis =
            Time.posixToMillis expected

        timeMillis =
            Time.posixToMillis time
    in
    timeMillis < expectedMillis
