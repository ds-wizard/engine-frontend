module Wizard.Components.Questionnaire.QuestionnaireViewSettings exposing
    ( QuestionnaireViewSettings
    , all
    , anyHidden
    , decoder
    , default
    , encode
    , none
    , toggleAnsweredBy
    , toggleMetricValues
    , toggleNonDesirableQuestions
    , togglePhases
    , toggleTags
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias QuestionnaireViewSettings =
    { answeredBy : Bool
    , phases : Bool
    , tags : Bool
    , nonDesirableQuestions : Bool
    , metricValues : Bool
    }


decoder : Decoder QuestionnaireViewSettings
decoder =
    D.succeed QuestionnaireViewSettings
        |> D.required "answeredBy" D.bool
        |> D.required "phases" D.bool
        |> D.required "tags" D.bool
        |> D.required "nonDesirableQuestions" D.bool
        |> D.required "metricValues" D.bool


encode : QuestionnaireViewSettings -> E.Value
encode qvs =
    E.object
        [ ( "answeredBy", E.bool qvs.answeredBy )
        , ( "phases", E.bool qvs.phases )
        , ( "tags", E.bool qvs.tags )
        , ( "nonDesirableQuestions", E.bool qvs.nonDesirableQuestions )
        , ( "metricValues", E.bool qvs.metricValues )
        ]


all : QuestionnaireViewSettings
all =
    { answeredBy = True
    , phases = True
    , tags = True
    , nonDesirableQuestions = True
    , metricValues = True
    }


default : QuestionnaireViewSettings
default =
    all


none : QuestionnaireViewSettings
none =
    { answeredBy = False
    , phases = False
    , tags = False
    , nonDesirableQuestions = False
    , metricValues = False
    }


anyHidden : QuestionnaireViewSettings -> Bool
anyHidden qvs =
    not qvs.answeredBy || not qvs.phases || not qvs.tags || not qvs.nonDesirableQuestions || not qvs.metricValues


toggleAnsweredBy : QuestionnaireViewSettings -> QuestionnaireViewSettings
toggleAnsweredBy qvs =
    { qvs | answeredBy = not qvs.answeredBy }


togglePhases : QuestionnaireViewSettings -> QuestionnaireViewSettings
togglePhases qvs =
    { qvs | phases = not qvs.phases }


toggleTags : QuestionnaireViewSettings -> QuestionnaireViewSettings
toggleTags qvs =
    { qvs | tags = not qvs.tags }


toggleNonDesirableQuestions : QuestionnaireViewSettings -> QuestionnaireViewSettings
toggleNonDesirableQuestions qvs =
    { qvs | nonDesirableQuestions = not qvs.nonDesirableQuestions }


toggleMetricValues : QuestionnaireViewSettings -> QuestionnaireViewSettings
toggleMetricValues qvs =
    { qvs | metricValues = not qvs.metricValues }
