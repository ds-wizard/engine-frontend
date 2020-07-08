module Shared.Data.Questionnaire.QuestionnaireReport exposing
    ( QuestionnaireReport
    , decoder
    )

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D
import Shared.Data.SummaryReport exposing (IndicationReport, indicationReportDecoder)


type alias QuestionnaireReport =
    { indications : List IndicationReport
    }


decoder : Decoder { indications : List IndicationReport }
decoder =
    D.succeed QuestionnaireReport
        |> D.required "indications" (D.list indicationReportDecoder)
