module Common.Components.Questionnaire.ViewSettings exposing
    ( ViewSettings
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

{-| What the questionnaire's "View" menu shows or hides. Stored in local storage,
so a user's choice carries over.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ViewSettings =
    { answeredBy : Bool
    , metricValues : Bool
    , nonDesirableQuestions : Bool
    , phases : Bool
    , tags : Bool
    }


default : ViewSettings
default =
    { answeredBy = True
    , metricValues = True
    , nonDesirableQuestions = True
    , phases = True
    , tags = True
    }


decoder : Decoder ViewSettings
decoder =
    D.succeed ViewSettings
        |> D.required "answeredBy" D.bool
        |> D.required "metricValues" D.bool
        |> D.required "nonDesirableQuestions" D.bool
        |> D.required "phases" D.bool
        |> D.required "tags" D.bool


encode : ViewSettings -> E.Value
encode settings =
    E.object
        [ ( "answeredBy", E.bool settings.answeredBy )
        , ( "metricValues", E.bool settings.metricValues )
        , ( "nonDesirableQuestions", E.bool settings.nonDesirableQuestions )
        , ( "phases", E.bool settings.phases )
        , ( "tags", E.bool settings.tags )
        ]


anyHidden : ViewSettings -> Bool
anyHidden settings =
    not settings.answeredBy
        || not settings.metricValues
        || not settings.nonDesirableQuestions
        || not settings.phases
        || not settings.tags


all : ViewSettings
all =
    { answeredBy = True
    , metricValues = True
    , nonDesirableQuestions = True
    , phases = True
    , tags = True
    }


none : ViewSettings
none =
    { answeredBy = False
    , metricValues = False
    , nonDesirableQuestions = False
    , phases = False
    , tags = False
    }


toggleAnsweredBy : ViewSettings -> ViewSettings
toggleAnsweredBy qvs =
    { qvs | answeredBy = not qvs.answeredBy }


togglePhases : ViewSettings -> ViewSettings
togglePhases qvs =
    { qvs | phases = not qvs.phases }


toggleTags : ViewSettings -> ViewSettings
toggleTags qvs =
    { qvs | tags = not qvs.tags }


toggleNonDesirableQuestions : ViewSettings -> ViewSettings
toggleNonDesirableQuestions qvs =
    { qvs | nonDesirableQuestions = not qvs.nonDesirableQuestions }


toggleMetricValues : ViewSettings -> ViewSettings
toggleMetricValues qvs =
    { qvs | metricValues = not qvs.metricValues }
