module Wizard.Components.Questionnaire2.QuestionnaireViewSettings exposing
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
    , metricValues : Bool
    , nonDesirableQuestions : Bool
    , phases : Bool
    , tags : Bool
    }


default : QuestionnaireViewSettings
default =
    { answeredBy = True
    , metricValues = True
    , nonDesirableQuestions = True
    , phases = True
    , tags = True
    }


decoder : Decoder QuestionnaireViewSettings
decoder =
    D.succeed QuestionnaireViewSettings
        |> D.required "answeredBy" D.bool
        |> D.required "metricValues" D.bool
        |> D.required "nonDesirableQuestions" D.bool
        |> D.required "phases" D.bool
        |> D.required "tags" D.bool


encode : QuestionnaireViewSettings -> E.Value
encode settings =
    E.object
        [ ( "answeredBy", E.bool settings.answeredBy )
        , ( "metricValues", E.bool settings.metricValues )
        , ( "nonDesirableQuestions", E.bool settings.nonDesirableQuestions )
        , ( "phases", E.bool settings.phases )
        , ( "tags", E.bool settings.tags )
        ]


anyHidden : QuestionnaireViewSettings -> Bool
anyHidden settings =
    not settings.answeredBy
        || not settings.metricValues
        || not settings.nonDesirableQuestions
        || not settings.phases
        || not settings.tags


all : QuestionnaireViewSettings
all =
    { answeredBy = True
    , metricValues = True
    , nonDesirableQuestions = True
    , phases = True
    , tags = True
    }


none : QuestionnaireViewSettings
none =
    { answeredBy = False
    , metricValues = False
    , nonDesirableQuestions = False
    , phases = False
    , tags = False
    }


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
