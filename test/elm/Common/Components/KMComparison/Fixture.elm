module Common.Components.KMComparison.Fixture exposing
    ( answer
    , chapter
    , km
    , measure
    , metric
    , optionsQuestion
    , valueQuestion
    , withAnswers
    , withChapters
    , withFollowUps
    , withMetrics
    , withQuestions
    )

{-| Knowledge models built in the test, a piece at a time.

The comparison is a pure function of two knowledge models, so its tests need no
JSON, no server and no browser — only two values that differ in the one way the
test is about.

The `with…` builders insert into a parent that must already be there, so a
knowledge model is written in the order it is read.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Common.Api.Models.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Common.Api.Models.KnowledgeModel.Question exposing (Question(..))
import Common.Api.Models.KnowledgeModel.Question.CommonQuestionData exposing (CommonQuestionData)
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))


km : KnowledgeModel
km =
    KnowledgeModel.empty


chapter : String -> String -> Chapter
chapter uuid title =
    { uuid = uuid, title = title, text = Nothing, questionUuids = [], annotations = [] }


valueQuestion : String -> String -> Question
valueQuestion uuid title =
    ValueQuestion (commonData uuid title) { valueType = StringQuestionValueType, validations = [] }


optionsQuestion : String -> String -> Question
optionsQuestion uuid title =
    OptionsQuestion (commonData uuid title) { answerUuids = [] }


commonData : String -> String -> CommonQuestionData
commonData uuid title =
    { uuid = uuid
    , title = title
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , tagUuids = []
    , referenceUuids = []
    , expertUuids = []
    , annotations = []
    }


answer : String -> String -> List MetricMeasure -> Answer
answer uuid label metricMeasures =
    { uuid = uuid
    , label = label
    , advice = Nothing
    , metricMeasures = metricMeasures
    , followUpUuids = []
    , annotations = []
    }


measure : String -> Float -> MetricMeasure
measure metricUuid value =
    { metricUuid = metricUuid, measure = value, weight = 1 }


metric : String -> String -> Metric
metric uuid title =
    { uuid = uuid, title = title, abbreviation = Nothing, description = Nothing, annotations = [] }


withChapters : List Chapter -> KnowledgeModel -> KnowledgeModel
withChapters chapters knowledgeModel =
    List.foldl (\c -> KnowledgeModel.insertChapter c "") knowledgeModel chapters


withQuestions : String -> List Question -> KnowledgeModel -> KnowledgeModel
withQuestions parentUuid questions knowledgeModel =
    List.foldl (\q -> KnowledgeModel.insertQuestion q parentUuid) knowledgeModel questions


withAnswers : String -> List Answer -> KnowledgeModel -> KnowledgeModel
withAnswers questionUuid answers knowledgeModel =
    List.foldl (\a -> KnowledgeModel.insertAnswer a questionUuid) knowledgeModel answers


withFollowUps : String -> List Question -> KnowledgeModel -> KnowledgeModel
withFollowUps answerUuid questions knowledgeModel =
    List.foldl (\q -> KnowledgeModel.insertQuestion q answerUuid) knowledgeModel questions


withMetrics : List Metric -> KnowledgeModel -> KnowledgeModel
withMetrics metrics knowledgeModel =
    List.foldl (\m -> KnowledgeModel.insertMetric m "") knowledgeModel metrics
