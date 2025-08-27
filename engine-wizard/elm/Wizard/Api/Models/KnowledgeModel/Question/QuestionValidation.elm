module Wizard.Api.Models.KnowledgeModel.Question.QuestionValidation exposing
    ( QuestionValidation(..)
    , QuestionValidationData
    , decoder
    , doi
    , domain
    , encode
    , fromDate
    , fromDateTime
    , fromTime
    , maxLength
    , maxNumber
    , minLength
    , minNumber
    , orcid
    , regex
    , toDate
    , toDateTime
    , toOptionString
    , toTime
    , validate
    )

import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import List.Extra as List
import Regex
import Shared.Data.DateTimeString as DateTimeString
import Shared.Utils.RegexPatterns as RegexPatterns
import String.Format as String


type QuestionValidation
    = MinLength (QuestionValidationData Int)
    | MaxLength (QuestionValidationData Int)
    | Regex (QuestionValidationData String)
    | Orcid
    | Doi
    | MinNumber (QuestionValidationData Float)
    | MaxNumber (QuestionValidationData Float)
    | FromDate (QuestionValidationData String)
    | ToDate (QuestionValidationData String)
    | FromDateTime (QuestionValidationData String)
    | ToDateTime (QuestionValidationData String)
    | FromTime (QuestionValidationData String)
    | ToTime (QuestionValidationData String)
    | Domain (QuestionValidationData String)


minLength : QuestionValidation
minLength =
    MinLength { value = 10 }


maxLength : QuestionValidation
maxLength =
    MaxLength { value = 10 }


regex : QuestionValidation
regex =
    Regex { value = "" }


orcid : QuestionValidation
orcid =
    Orcid


doi : QuestionValidation
doi =
    Doi


minNumber : QuestionValidation
minNumber =
    MinNumber { value = 10.0 }


maxNumber : QuestionValidation
maxNumber =
    MaxNumber { value = 10.0 }


fromDate : QuestionValidation
fromDate =
    FromDate { value = "" }


toDate : QuestionValidation
toDate =
    ToDate { value = "" }


fromDateTime : QuestionValidation
fromDateTime =
    FromDateTime { value = "" }


toDateTime : QuestionValidation
toDateTime =
    ToDateTime { value = "" }


fromTime : QuestionValidation
fromTime =
    FromTime { value = "" }


toTime : QuestionValidation
toTime =
    ToTime { value = "" }


domain : QuestionValidation
domain =
    Domain { value = "" }


toOptionString : QuestionValidation -> String
toOptionString questionValidation =
    case questionValidation of
        MinLength _ ->
            "MinLength"

        MaxLength _ ->
            "MaxLength"

        Regex _ ->
            "Regex"

        Orcid ->
            "Orcid"

        Doi ->
            "Doi"

        MinNumber _ ->
            "MinNumber"

        MaxNumber _ ->
            "MaxNumber"

        FromDate _ ->
            "FromDate"

        ToDate _ ->
            "ToDate"

        FromDateTime _ ->
            "FromDateTime"

        ToDateTime _ ->
            "ToDateTime"

        FromTime _ ->
            "FromTime"

        ToTime _ ->
            "ToTime"

        Domain _ ->
            "Domain"


type alias QuestionValidationData a =
    { value : a }


decoder : Decoder QuestionValidation
decoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "MinLengthQuestionValidation" ->
                        dataDecoder D.int
                            |> D.map MinLength

                    "MaxLengthQuestionValidation" ->
                        dataDecoder D.int
                            |> D.map MaxLength

                    "RegexQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map Regex

                    "OrcidQuestionValidation" ->
                        D.succeed Orcid

                    "DoiQuestionValidation" ->
                        D.succeed Doi

                    "MinNumberQuestionValidation" ->
                        dataDecoder D.float
                            |> D.map MinNumber

                    "MaxNumberQuestionValidation" ->
                        dataDecoder D.float
                            |> D.map MaxNumber

                    "FromDateQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map FromDate

                    "ToDateQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map ToDate

                    "FromDateTimeQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map FromDateTime

                    "ToDateTimeQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map ToDateTime

                    "FromTimeQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map FromTime

                    "ToTimeQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map ToTime

                    "DomainQuestionValidation" ->
                        dataDecoder D.string
                            |> D.map Domain

                    _ ->
                        D.fail ("Unknown QuestionValidation type: " ++ type_)
            )


dataDecoder : Decoder a -> Decoder (QuestionValidationData a)
dataDecoder valueDecoder =
    D.succeed QuestionValidationData
        |> D.required "value" valueDecoder


encode : QuestionValidation -> E.Value
encode questionValidation =
    case questionValidation of
        MinLength data ->
            encodeValidationWithData "MinLengthQuestionValidation" E.int data

        MaxLength data ->
            encodeValidationWithData "MaxLengthQuestionValidation" E.int data

        Regex data ->
            encodeValidationWithData "RegexQuestionValidation" E.string data

        Orcid ->
            encodeValidation "OrcidQuestionValidation"

        Doi ->
            encodeValidation "DoiQuestionValidation"

        MinNumber data ->
            encodeValidationWithData "MinNumberQuestionValidation" E.float data

        MaxNumber data ->
            encodeValidationWithData "MaxNumberQuestionValidation" E.float data

        FromDate data ->
            encodeValidationWithData "FromDateQuestionValidation" E.string data

        ToDate data ->
            encodeValidationWithData "ToDateQuestionValidation" E.string data

        FromDateTime data ->
            encodeValidationWithData "FromDateTimeQuestionValidation" E.string data

        ToDateTime data ->
            encodeValidationWithData "ToDateTimeQuestionValidation" E.string data

        FromTime data ->
            encodeValidationWithData "FromTimeQuestionValidation" E.string data

        ToTime data ->
            encodeValidationWithData "ToTimeQuestionValidation" E.string data

        Domain data ->
            encodeValidationWithData "DomainQuestionValidation" E.string data


encodeValidation : String -> E.Value
encodeValidation type_ =
    E.object
        [ ( "type", E.string type_ ) ]


encodeValidationWithData : String -> (a -> E.Value) -> QuestionValidationData a -> E.Value
encodeValidationWithData type_ valueEncoder data =
    E.object
        [ ( "type", E.string type_ )
        , ( "value", valueEncoder data.value )
        ]


validate : { a | locale : Gettext.Locale } -> QuestionValidation -> String -> Result String ()
validate appState validation value =
    case validation of
        MinLength data ->
            if String.length value >= data.value then
                Ok ()

            else
                Err <| String.format (gettext "Answer must be at least %s characters long." appState.locale) [ String.fromInt data.value ]

        MaxLength data ->
            if String.length value <= data.value then
                Ok ()

            else
                Err <| String.format (gettext "Answer must be at most %s characters long." appState.locale) [ String.fromInt data.value ]

        Regex data ->
            if Regex.contains (RegexPatterns.fromString data.value) value then
                Ok ()

            else
                Err <| String.format (gettext "Answer does not match the required pattern (%s)." appState.locale) [ data.value ]

        Orcid ->
            if Regex.contains RegexPatterns.orcid value then
                Ok ()

            else
                Err <| gettext "Answer must be a valid ORCID." appState.locale

        Doi ->
            if Regex.contains RegexPatterns.doi value then
                Ok ()

            else
                Err <| gettext "Answer must be a valid DOI." appState.locale

        MinNumber data ->
            if Maybe.withDefault 0.0 (String.toFloat value) >= data.value then
                Ok ()

            else
                Err <| String.format (gettext "Answer must be at least %s." appState.locale) [ String.fromFloat data.value ]

        MaxNumber data ->
            if Maybe.withDefault 0.0 (String.toFloat value) <= data.value then
                Ok ()

            else
                Err <| String.format (gettext "Answer must be at most %s." appState.locale) [ String.fromFloat data.value ]

        FromDate data ->
            let
                selectedValue =
                    DateTimeString.date value

                validationValue =
                    DateTimeString.date data.value
            in
            if DateTimeString.dateGte selectedValue validationValue then
                Ok ()

            else
                Err <| String.format (gettext "Date must be at least %s." appState.locale) [ data.value ]

        ToDate data ->
            let
                selectedValue =
                    DateTimeString.date value

                validationValue =
                    DateTimeString.date data.value
            in
            if DateTimeString.dateLte selectedValue validationValue then
                Ok ()

            else
                Err <| String.format (gettext "Date must be at most %s." appState.locale) [ data.value ]

        FromDateTime data ->
            let
                selectedValue =
                    DateTimeString.dateTime value

                validationValue =
                    DateTimeString.dateTime data.value
            in
            if DateTimeString.dateTimeGte selectedValue validationValue then
                Ok ()

            else
                Err <| String.format (gettext "Date and time must be at least %s." appState.locale) [ data.value ]

        ToDateTime data ->
            let
                selectedValue =
                    DateTimeString.dateTime value

                validationValue =
                    DateTimeString.dateTime data.value
            in
            if DateTimeString.dateTimeLte selectedValue validationValue then
                Ok ()

            else
                Err <| String.format (gettext "Date and time must be at most %s." appState.locale) [ data.value ]

        FromTime data ->
            let
                selectedValue =
                    DateTimeString.time value

                validationValue =
                    DateTimeString.time data.value
            in
            if DateTimeString.timeGte selectedValue validationValue then
                Ok ()

            else
                Err <| String.format (gettext "Time must be at least %s." appState.locale) [ data.value ]

        ToTime data ->
            let
                selectedValue =
                    DateTimeString.time value

                validationValue =
                    DateTimeString.time data.value
            in
            if DateTimeString.timeLte selectedValue validationValue then
                Ok ()

            else
                Err <| String.format (gettext "Time must be at most %s." appState.locale) [ data.value ]

        Domain data ->
            -- Validate domain only for valid emails
            if Regex.contains RegexPatterns.email value then
                let
                    emailDomain =
                        String.split "@" value
                            |> List.last
                            |> Maybe.withDefault ""

                    allowedDomains =
                        List.map String.trim (String.split "," data.value)
                in
                if List.member emailDomain allowedDomains then
                    Ok ()

                else
                    Err <| String.format (gettext "Email domain must be one of the following: %s." appState.locale) [ data.value ]

            else
                Ok ()
