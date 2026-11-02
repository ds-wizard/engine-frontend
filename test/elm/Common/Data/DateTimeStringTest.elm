module Common.Data.DateTimeStringTest exposing (dateTimeStringTests)

import Common.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation
import Common.Data.DateTimeString as DateTimeString
import Expect
import Gettext
import Test exposing (Test, describe, test)
import Time


validate : QuestionValidation.QuestionValidation -> String -> Bool
validate validation value =
    QuestionValidation.validate { locale = Gettext.defaultLocale, timeZone = Time.utc } validation value
        |> (==) (Ok ())


dateTimeStringTests : Test
dateTimeStringTests =
    describe "DateTimeString"
        [ test "reads the instant the date picker stores" <|
            \_ ->
                DateTimeString.dateTime "2024-05-01T10:30:00.000Z"
                    |> Expect.equal (DateTimeString.dateTime "2024-05-01 10:30")
        , test "an instant is not the empty one" <|
            \_ ->
                DateTimeString.dateTime "2024-05-01T10:30:00.000Z"
                    |> Expect.notEqual (DateTimeString.dateTime "")
        , test "from date time rejects an earlier instant" <|
            \_ ->
                validate (QuestionValidation.FromDateTime { value = "2024-05-01T10:30:00.000Z" }) "2024-04-30T23:59:00.000Z"
                    |> Expect.equal False
        , test "from date time accepts the same and a later instant" <|
            \_ ->
                ( validate (QuestionValidation.FromDateTime { value = "2024-05-01T10:30:00.000Z" }) "2024-05-01T10:30:00.000Z"
                , validate (QuestionValidation.FromDateTime { value = "2024-05-01T10:30:00.000Z" }) "2024-05-01T10:31:00.000Z"
                )
                    |> Expect.equal ( True, True )
        , test "to date time rejects a later instant" <|
            \_ ->
                validate (QuestionValidation.ToDateTime { value = "2024-05-01T10:30:00.000Z" }) "2024-05-01T11:00:00.000Z"
                    |> Expect.equal False
        , test "to date time accepts the same and an earlier instant" <|
            \_ ->
                ( validate (QuestionValidation.ToDateTime { value = "2024-05-01T10:30:00.000Z" }) "2024-05-01T10:30:00.000Z"
                , validate (QuestionValidation.ToDateTime { value = "2024-05-01T10:30:00.000Z" }) "2023-12-31T10:30:00.000Z"
                )
                    |> Expect.equal ( True, True )
        , test "the date and time limit reads in the reader's zone" <|
            \_ ->
                QuestionValidation.validate { locale = Gettext.defaultLocale, timeZone = Time.customZone 120 [] }
                    (QuestionValidation.FromDateTime { value = "2026-10-12T10:00:00.000Z" })
                    "2026-10-11T10:00:00.000Z"
                    |> Expect.equal (Err "Date and time must be on or after 12. 10. 2026, 12:00.")
        , test "the date limit reads as the calendar date" <|
            \_ ->
                QuestionValidation.validate { locale = Gettext.defaultLocale, timeZone = Time.customZone -600 [] }
                    (QuestionValidation.ToDate { value = "2026-10-12" })
                    "2026-10-13"
                    |> Expect.equal (Err "Date must be on or before 12. 10. 2026.")
        ]
