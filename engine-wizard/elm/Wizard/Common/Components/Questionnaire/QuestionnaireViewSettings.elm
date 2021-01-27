module Wizard.Common.Components.Questionnaire.QuestionnaireViewSettings exposing
    ( QuestionnaireViewSettings
    , all
    , none
    , toggleAnsweredBy
    , togglePhases
    , toggleTags
    )


type alias QuestionnaireViewSettings =
    { answeredBy : Bool
    , phases : Bool
    , tags : Bool
    }


all : QuestionnaireViewSettings
all =
    { answeredBy = True
    , phases = True
    , tags = True
    }


none : QuestionnaireViewSettings
none =
    { answeredBy = False
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
