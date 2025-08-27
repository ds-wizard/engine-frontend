module Shared.Data.DateTimeString exposing
    ( Date
    , DateTime
    , Time
    , date
    , dateGte
    , dateLte
    , dateTime
    , dateTimeGte
    , dateTimeLte
    , time
    , timeGte
    , timeLte
    )


type Date
    = Date Int Int Int


date : String -> Date
date str =
    case String.split "-" str of
        year :: month :: day :: [] ->
            Date
                (Maybe.withDefault 0 (String.toInt year))
                (Maybe.withDefault 0 (String.toInt month))
                (Maybe.withDefault 0 (String.toInt day))

        _ ->
            Date 0 0 0


dateGte : Date -> Date -> Bool
dateGte (Date y1 m1 d1) (Date y2 m2 d2) =
    (y1 > y1)
        || (y1 == y2 && m1 > m2)
        || (y1 == y2 && m1 == m2 && d1 >= d2)


dateGt : Date -> Date -> Bool
dateGt (Date y1 m1 d1) (Date y2 m2 d2) =
    (y1 > y1)
        || (y1 == y2 && m1 > m2)
        || (y1 == y2 && m1 == m2 && d1 > d2)


dateLte : Date -> Date -> Bool
dateLte (Date y1 m1 d1) (Date y2 m2 d2) =
    (y1 < y1)
        || (y1 == y2 && m1 < m2)
        || (y1 == y2 && m1 == m2 && d1 <= d2)


dateLt : Date -> Date -> Bool
dateLt (Date y1 m1 d1) (Date y2 m2 d2) =
    (y1 < y1)
        || (y1 == y2 && m1 < m2)
        || (y1 == y2 && m1 == m2 && d1 < d2)


type Time
    = Time Int Int


time : String -> Time
time str =
    case String.split ":" str of
        hour :: minute :: [] ->
            Time
                (Maybe.withDefault 0 (String.toInt hour))
                (Maybe.withDefault 0 (String.toInt minute))

        _ ->
            Time 0 0


timeGte : Time -> Time -> Bool
timeGte (Time h1 m1) (Time h2 m2) =
    (h1 > h2)
        || (h1 == h2 && m1 >= m2)


timeLte : Time -> Time -> Bool
timeLte (Time h1 m1) (Time h2 m2) =
    (h1 < h2)
        || (h1 == h2 && m1 <= m2)


type DateTime
    = DateTime Date Time


dateTime : String -> DateTime
dateTime str =
    case String.split " " str of
        dateStr :: timeStr :: [] ->
            DateTime (date dateStr) (time timeStr)

        _ ->
            DateTime (Date 0 0 0) (Time 0 0)


dateTimeGte : DateTime -> DateTime -> Bool
dateTimeGte (DateTime date1 time1) (DateTime date2 time2) =
    dateGt date1 date2
        || (date1 == date2 && timeGte time1 time2)


dateTimeLte : DateTime -> DateTime -> Bool
dateTimeLte (DateTime date1 time1) (DateTime date2 time2) =
    dateLt date1 date2
        || (date1 == date2 && timeLte time1 time2)
