module KMEditor.Editor.Models.Forms exposing (AnswerForm, ChapterForm, ExpertForm, KnowledgeModelForm, MetricMeasureForm, MetricMeasureValues, QuestionForm, ReferenceForm, ReferenceFormType(..), answerFormInitials, answerFormValidation, chapterFormInitials, chapterFormValidation, expertFormInitials, expertFormValidation, formChanged, getMetricMesures, initAnswerForm, initChapterForm, initExpertForm, initForm, initKnowledgeModelFrom, initQuestionForm, initReferenceForm, knowledgeModelFormInitials, knowledgeModelFormValidation, metricMeasureFormInitials, metricMeasureFormToMetricMeasure, metricMeasureValidation, questionFormInitials, questionFormValidation, questionTypeOptions, referenceFormInitials, referenceFormValidation, referenceTypeOptions, updateAnswerWithForm, updateChapterWithForm, updateExpertWithForm, updateKnowledgeModelWithForm, updateQuestionWithForm, updateReferenceWithForm, validateMeasureValue, validateMetricMeasureValues, validateReference)

import Common.Form exposing (CustomFormError)
import Common.Form.Validate exposing (validateUuid)
import Form exposing (Form)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Editor.Models.EditorContext exposing (EditorContext)
import List.Extra as List
import Set
import String exposing (fromInt, fromFloat)


type alias KnowledgeModelForm =
    { name : String }


type alias ChapterForm =
    { title : String
    , text : String
    }


type alias QuestionForm =
    { title : String
    , type_ : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , itemName : String
    }


type alias AnswerForm =
    { label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasureForm
    }


type alias MetricMeasureForm =
    { enabled : Bool
    , metricUuid : String
    , values : Maybe MetricMeasureValues
    }


type alias MetricMeasureValues =
    { weight : Float
    , measure : Float
    }


type ReferenceFormType
    = ResourcePageReferenceFormType String
    | URLReferenceFormType String String
    | CrossReferenceFormType String String


type alias ReferenceForm =
    { reference : ReferenceFormType
    }


type alias ExpertForm =
    { name : String
    , email : String
    }



{- Common utils -}


initForm : Validation CustomFormError a -> List ( String, Field.Field ) -> Form CustomFormError a
initForm validation initials =
    Form.initial initials validation


formChanged : Form CustomFormError a -> Bool
formChanged form =
    Set.size (Form.getChangedFields form) > 0



{- Knowledge Model -}


initKnowledgeModelFrom : KnowledgeModel -> Form CustomFormError KnowledgeModelForm
initKnowledgeModelFrom =
    knowledgeModelFormInitials >> initForm knowledgeModelFormValidation


knowledgeModelFormValidation : Validation CustomFormError KnowledgeModelForm
knowledgeModelFormValidation =
    Validate.map KnowledgeModelForm
        (Validate.field "name" Validate.string)


knowledgeModelFormInitials : KnowledgeModel -> List ( String, Field.Field )
knowledgeModelFormInitials knowledgeModel =
    [ ( "name", Field.string knowledgeModel.name ) ]


updateKnowledgeModelWithForm : KnowledgeModel -> KnowledgeModelForm -> KnowledgeModel
updateKnowledgeModelWithForm knowledgeModel knowledgeModelForm =
    { knowledgeModel | name = knowledgeModelForm.name }



{- Chapter -}


initChapterForm : Chapter -> Form CustomFormError ChapterForm
initChapterForm =
    chapterFormInitials >> initForm chapterFormValidation


chapterFormValidation : Validation CustomFormError ChapterForm
chapterFormValidation =
    Validate.map2 ChapterForm
        (Validate.field "title" Validate.string)
        (Validate.field "text" Validate.string)


chapterFormInitials : Chapter -> List ( String, Field.Field )
chapterFormInitials chapter =
    [ ( "title", Field.string chapter.title )
    , ( "text", Field.string chapter.text )
    ]


updateChapterWithForm : Chapter -> ChapterForm -> Chapter
updateChapterWithForm chapter chapterForm =
    { chapter | title = chapterForm.title, text = chapterForm.text }



{- Question -}


initQuestionForm : Question -> Form CustomFormError QuestionForm
initQuestionForm =
    questionFormInitials >> initForm questionFormValidation


questionFormValidation : Validation CustomFormError QuestionForm
questionFormValidation =
    Validate.map5 QuestionForm
        (Validate.field "title" Validate.string)
        (Validate.field "type_" Validate.string)
        (Validate.field "text" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
        (Validate.field "requiredLevel" (Validate.maybe Validate.int))
        (Validate.field "itemName" (Validate.oneOf [ Validate.emptyString, Validate.string ]))


questionFormInitials : Question -> List ( String, Field.Field )
questionFormInitials question =
    [ ( "title", Field.string question.title )
    , ( "type_", Field.string question.type_ )
    , ( "text", Field.string (question.text |> Maybe.withDefault "") )
    , ( "requiredLevel", Field.string (question.requiredLevel |> Maybe.map fromInt |> Maybe.withDefault "") )
    , ( "itemName", Field.string (question.answerItemTemplate |> Maybe.map .title |> Maybe.withDefault "") )
    ]


updateQuestionWithForm : Question -> QuestionForm -> Question
updateQuestionWithForm question questionForm =
    let
        answerItemTemplate =
            if questionForm.type_ == "list" then
                Just
                    { title = questionForm.itemName
                    , questions = AnswerItemTemplateQuestions []
                    }
            else
                Nothing
    in
    { question
        | title = questionForm.title
        , text = questionForm.text
        , type_ = questionForm.type_
        , requiredLevel = questionForm.requiredLevel
        , answerItemTemplate = answerItemTemplate
    }


questionTypeOptions : List ( String, String )
questionTypeOptions =
    [ ( "options", "Options" )
    , ( "list", "List of items" )
    , ( "string", "String" )
    , ( "number", "Number" )
    , ( "date", "Date" )
    , ( "text", "Long text" )
    ]



{- Answer -}


initAnswerForm : EditorContext -> Answer -> Form CustomFormError AnswerForm
initAnswerForm editorContext =
    answerFormInitials editorContext >> initForm answerFormValidation


answerFormValidation : Validation CustomFormError AnswerForm
answerFormValidation =
    Validate.map3 AnswerForm
        (Validate.field "label" Validate.string)
        (Validate.field "advice" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
        (Validate.field "metricMeasures" (Validate.list metricMeasureValidation))


metricMeasureValidation : Validation CustomFormError MetricMeasureForm
metricMeasureValidation =
    Validate.map3 MetricMeasureForm
        (Validate.field "enabled" Validate.bool)
        (Validate.field "metricUuid" Validate.string)
        (Validate.field "enabled" Validate.bool |> Validate.andThen validateMetricMeasureValues)


validateMetricMeasureValues : Bool -> Validation CustomFormError (Maybe MetricMeasureValues)
validateMetricMeasureValues enabled =
    if enabled then
        Validate.succeed MetricMeasureValues
            |> Validate.andMap (Validate.field "weight" validateMeasureValue)
            |> Validate.andMap (Validate.field "measure" validateMeasureValue)
            |> map Just
    else
        Validate.succeed Nothing


validateMeasureValue : Validation e Float
validateMeasureValue =
    Validate.float
        |> Validate.andThen (Validate.minFloat 0)
        |> Validate.andThen (Validate.maxFloat 1)


answerFormInitials : EditorContext -> Answer -> List ( String, Field.Field )
answerFormInitials editorContext answer =
    [ ( "label", Field.string answer.label )
    , ( "advice", Field.string (answer.advice |> Maybe.withDefault "") )
    , ( "metricMeasures", Field.list (List.map (metricMeasureFormInitials answer.metricMeasures) editorContext.metrics) )
    ]


metricMeasureFormInitials : List MetricMeasure -> Metric -> Field.Field
metricMeasureFormInitials metricMeasures metric =
    case List.find (.metricUuid >> (==) metric.uuid) metricMeasures of
        Just metricMeasure ->
            Field.group
                [ ( "enabled", Field.bool True )
                , ( "metricUuid", Field.string metric.uuid )
                , ( "weight", Field.string (fromFloat metricMeasure.weight) )
                , ( "measure", Field.string (fromFloat metricMeasure.measure) )
                ]

        Nothing ->
            Field.group
                [ ( "enabled", Field.bool False )
                , ( "metricUuid", Field.string metric.uuid )
                , ( "weight", Field.string (fromFloat 1) )
                , ( "measure", Field.string (fromFloat 1) )
                ]


updateAnswerWithForm : Answer -> AnswerForm -> Answer
updateAnswerWithForm answer answerForm =
    { answer
        | label = answerForm.label
        , advice = answerForm.advice
        , metricMeasures = getMetricMesures answerForm
    }


getMetricMesures : AnswerForm -> List MetricMeasure
getMetricMesures answerForm =
    answerForm.metricMeasures
        |> List.filter .enabled
        |> List.map metricMeasureFormToMetricMeasure


metricMeasureFormToMetricMeasure : MetricMeasureForm -> MetricMeasure
metricMeasureFormToMetricMeasure form =
    { metricUuid = form.metricUuid
    , measure = form.values |> Maybe.map .measure |> Maybe.withDefault 0
    , weight = form.values |> Maybe.map .weight |> Maybe.withDefault 0
    }



{- Reference -}


initReferenceForm : Reference -> Form CustomFormError ReferenceForm
initReferenceForm =
    referenceFormInitials >> initForm referenceFormValidation


referenceFormValidation : Validation CustomFormError ReferenceForm
referenceFormValidation =
    Validate.succeed ReferenceForm
        |> Validate.andMap (Validate.field "referenceType" Validate.string |> Validate.andThen validateReference)


validateReference : String -> Validation CustomFormError ReferenceFormType
validateReference referenceType =
    case referenceType of
        "ResourcePageReference" ->
            Validate.succeed ResourcePageReferenceFormType
                |> Validate.andMap (Validate.field "shortUuid" Validate.string)

        "URLReference" ->
            Validate.succeed URLReferenceFormType
                |> Validate.andMap (Validate.field "url" Validate.string)
                |> Validate.andMap (Validate.field "label" Validate.string)

        "CrossReference" ->
            Validate.succeed CrossReferenceFormType
                |> Validate.andMap (Validate.field "targetUuid" validateUuid)
                |> Validate.andMap (Validate.field "description" Validate.string)

        _ ->
            Validate.fail <| Error.value InvalidString


referenceFormInitials : Reference -> List ( String, Field.Field )
referenceFormInitials reference =
    case reference of
        ResourcePageReference data ->
            [ ( "referenceType", Field.string "ResourcePageReference" )
            , ( "shortUuid", Field.string data.shortUuid )
            ]

        URLReference data ->
            [ ( "referenceType", Field.string "URLReference" )
            , ( "url", Field.string data.url )
            , ( "label", Field.string data.label )
            ]

        CrossReference data ->
            [ ( "referenceType", Field.string "CrossReference" )
            , ( "targetUuid", Field.string data.targetUuid )
            , ( "description", Field.string data.description )
            ]


updateReferenceWithForm : Reference -> ReferenceForm -> Reference
updateReferenceWithForm reference referenceForm =
    case referenceForm.reference of
        ResourcePageReferenceFormType shortUuid ->
            ResourcePageReference
                { uuid = getReferenceUuid reference
                , shortUuid = shortUuid
                }

        URLReferenceFormType url label ->
            URLReference
                { uuid = getReferenceUuid reference
                , url = url
                , label = label
                }

        CrossReferenceFormType targetUuid description ->
            CrossReference
                { uuid = getReferenceUuid reference
                , targetUuid = targetUuid
                , description = description
                }


referenceTypeOptions : List ( String, String )
referenceTypeOptions =
    [ ( "ResourcePageReference", "Resource Page" )
    , ( "URLReference", "URL" )
    , ( "CrossReference", "Cross Reference" )
    ]



{- Expert -}


initExpertForm : Expert -> Form CustomFormError ExpertForm
initExpertForm =
    expertFormInitials >> initForm expertFormValidation


expertFormValidation : Validation CustomFormError ExpertForm
expertFormValidation =
    Validate.map2 ExpertForm
        (Validate.field "name" Validate.string)
        (Validate.field "email" Validate.email)


expertFormInitials : Expert -> List ( String, Field.Field )
expertFormInitials expert =
    [ ( "name", Field.string expert.name )
    , ( "email", Field.string expert.email )
    ]


updateExpertWithForm : Expert -> ExpertForm -> Expert
updateExpertWithForm expert expertForm =
    { expert | name = expertForm.name, email = expertForm.email }
