module Shared.Data.KnowledgeModel.Question.QuestionValidation exposing
    ( QuestionValidation(..)
    , QuestionValidationData
    , decoder
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
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


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
