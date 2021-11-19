module Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing
    ( AnswerForm
    , ChapterForm
    , ChoiceForm
    , ExpertForm
    , IntegrationForm
    , KnowledgeModelForm
    , MetricForm
    , MetricMeasureForm
    , PhaseForm
    , QuestionForm
    , QuestionFormType(..)
    , ReferenceForm
    , ReferenceFormType(..)
    , TagForm
    , answerFormValidation
    , chapterFormValidation
    , choiceFormValidation
    , expertFormValidation
    , formChanged
    , getMetricMeasures
    , initAnswerForm
    , initChapterForm
    , initChoiceForm
    , initExpertForm
    , initForm
    , initIntegrationForm
    , initKnowledgeModelFrom
    , initMetricForm
    , initPhaseForm
    , initQuestionForm
    , initReferenceForm
    , initTagForm
    , integrationFormValidation
    , isListQuestionForm
    , isMultiChoiceQuestionForm
    , isOptionsQuestionForm
    , knowledgeModelFormValidation
    , metricFormValidation
    , phaseFormValidation
    , questionFormValidation
    , questionTypeOptions
    , questionValueTypeOptions
    , referenceFormValidation
    , referenceTypeOptions
    , tagFormValidation
    , updateAnswerWithForm
    , updateChapterWithForm
    , updateChoiceWithForm
    , updateExpertWithForm
    , updateIntegrationWithForm
    , updateKnowledgeModelWithForm
    , updateMetricWithForm
    , updatePhaseWithForm
    , updateQuestionWithForm
    , updateReferenceWithForm
    , updateTagWithForm
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import List.Extra as List
import Set
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError(..))
import Shared.Form.Validate as V
import Shared.Locale exposing (lg)
import String exposing (fromFloat)
import Wizard.Common.AppState exposing (AppState)


type alias KnowledgeModelForm =
    { annotations : List ( String, String ) }


type alias MetricForm =
    { title : String
    , abbreviation : Maybe String
    , description : Maybe String
    , annotations : List ( String, String )
    }


type alias PhaseForm =
    { title : String
    , description : Maybe String
    , annotations : List ( String, String )
    }


type alias TagForm =
    { name : String
    , description : Maybe String
    , color : String
    , annotations : List ( String, String )
    }


type alias IntegrationForm =
    { id : String
    , name : String
    , logo : String
    , requestMethod : String
    , requestUrl : String
    , requestHeaders : List ( String, String )
    , requestBody : String
    , responseListField : String
    , responseItemId : String
    , responseItemTemplate : String
    , responseItemUrl : String
    , annotations : List ( String, String )
    }


type alias ChapterForm =
    { title : String
    , text : Maybe String
    , annotations : List ( String, String )
    }


type alias QuestionForm =
    { question : QuestionFormType }


type QuestionFormType
    = OptionsQuestionForm OptionsQuestionFormData
    | ListQuestionForm ListQuestionFormData
    | ValueQuestionForm ValueQuestionFormData
    | IntegrationQuestionForm IntegrationQuestionFormData
    | MultiChoiceQuestionForm MultiChoiceQuestionFormData


type alias OptionsQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredPhase : Maybe String
    , annotations : List ( String, String )
    }


type alias ListQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredPhase : Maybe String
    , annotations : List ( String, String )
    }


type alias ValueQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredPhase : Maybe String
    , valueType : QuestionValueType
    , annotations : List ( String, String )
    }


type alias IntegrationQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredPhase : Maybe String
    , integrationUuid : String
    , props : Dict String String
    , annotations : List ( String, String )
    }


type alias MultiChoiceQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredPhase : Maybe String
    , annotations : List ( String, String )
    }


type alias AnswerForm =
    { label : String
    , advice : Maybe String
    , metricMeasures : List MetricMeasureForm
    , annotations : List ( String, String )
    }


type alias ChoiceForm =
    { label : String
    , annotations : List ( String, String )
    }


type alias MetricMeasureForm =
    { metricUuid : String
    , enabled : Bool
    , values : Maybe MetricMeasureValues
    }


type alias MetricMeasureValues =
    { weight : Float
    , measure : Float
    }


type ReferenceFormType
    = ResourcePageReferenceFormType String (List ( String, String ))
    | URLReferenceFormType String String (List ( String, String ))
    | CrossReferenceFormType String String (List ( String, String ))


type alias ReferenceForm =
    { reference : ReferenceFormType
    }


type alias ExpertForm =
    { name : String
    , email : String
    , annotations : List ( String, String )
    }



{- Common utils -}


initForm : Validation FormError a -> List ( String, Field.Field ) -> Form FormError a
initForm validation initials =
    Form.initial initials validation


formChanged : Form FormError a -> Bool
formChanged form =
    Set.size (Form.getChangedFields form) > 0



{- Knowledge Model -}


initKnowledgeModelFrom : KnowledgeModel -> Form FormError KnowledgeModelForm
initKnowledgeModelFrom =
    knowledgeModelFormInitials >> initForm knowledgeModelFormValidation


knowledgeModelFormValidation : Validation FormError KnowledgeModelForm
knowledgeModelFormValidation =
    V.succeed KnowledgeModelForm
        |> V.andMap (V.field "annotations" validateAnnotations)


knowledgeModelFormInitials : KnowledgeModel -> List ( String, Field.Field )
knowledgeModelFormInitials knowledgeModel =
    [ ( "annotations", annotationsField knowledgeModel.annotations ) ]


updateKnowledgeModelWithForm : KnowledgeModel -> KnowledgeModelForm -> KnowledgeModel
updateKnowledgeModelWithForm knowledgeModel knowledgeModelForm =
    { knowledgeModel | annotations = Dict.fromList knowledgeModelForm.annotations }



{- Metric -}


initMetricForm : Metric -> Form FormError MetricForm
initMetricForm =
    metricFormInitials >> initForm metricFormValidation


metricFormValidation : Validation FormError MetricForm
metricFormValidation =
    V.succeed MetricForm
        |> V.andMap (V.field "title" V.string)
        |> V.andMap (V.field "abbreviation" V.maybeString)
        |> V.andMap (V.field "description" V.maybeString)
        |> V.andMap (V.field "annotations" validateAnnotations)


metricFormInitials : Metric -> List ( String, Field.Field )
metricFormInitials metric =
    [ ( "title", Field.string metric.title )
    , ( "abbreviation", Field.maybeString metric.abbreviation )
    , ( "description", Field.maybeString metric.description )
    , ( "annotations", annotationsField metric.annotations )
    ]


updateMetricWithForm : Metric -> MetricForm -> Metric
updateMetricWithForm metric metricForm =
    { metric
        | title = metricForm.title
        , abbreviation = metricForm.abbreviation
        , description = metricForm.description
        , annotations = Dict.fromList metricForm.annotations
    }



{- Phase -}


initPhaseForm : Phase -> Form FormError PhaseForm
initPhaseForm =
    phaseFormInitials >> initForm phaseFormValidation


phaseFormValidation : Validation FormError PhaseForm
phaseFormValidation =
    V.succeed PhaseForm
        |> V.andMap (V.field "title" V.string)
        |> V.andMap (V.field "description" V.maybeString)
        |> V.andMap (V.field "annotations" validateAnnotations)


phaseFormInitials : Phase -> List ( String, Field.Field )
phaseFormInitials phase =
    [ ( "title", Field.string phase.title )
    , ( "description", Field.maybeString phase.description )
    , ( "annotations", annotationsField phase.annotations )
    ]


updatePhaseWithForm : Phase -> PhaseForm -> Phase
updatePhaseWithForm phase phaseForm =
    { phase
        | title = phaseForm.title
        , annotations = Dict.fromList phaseForm.annotations
    }



{- Tag -}


initTagForm : Tag -> Form FormError TagForm
initTagForm =
    tagFormInitials >> initForm tagFormValidation


tagFormValidation : Validation FormError TagForm
tagFormValidation =
    V.succeed TagForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.maybeString)
        |> V.andMap (V.field "color" V.string)
        |> V.andMap (V.field "annotations" validateAnnotations)


tagFormInitials : Tag -> List ( String, Field.Field )
tagFormInitials tag =
    [ ( "name", Field.string tag.name )
    , ( "description", Field.maybeString tag.description )
    , ( "color", Field.string tag.color )
    , ( "annotations", annotationsField tag.annotations )
    ]


updateTagWithForm : Tag -> TagForm -> Tag
updateTagWithForm tag tagForm =
    { tag
        | name = tagForm.name
        , description = tagForm.description
        , color = tagForm.color
        , annotations = Dict.fromList tagForm.annotations
    }



{- Integration -}


initIntegrationForm : List Integration -> String -> Integration -> Form FormError IntegrationForm
initIntegrationForm integrations uuid =
    integrationFormInitials >> initForm (integrationFormValidation integrations uuid)


integrationFormValidation : List Integration -> String -> Validation FormError IntegrationForm
integrationFormValidation integrations uuid =
    V.succeed IntegrationForm
        |> V.andMap (V.field "id" (validateIntegrationId integrations uuid))
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "logo" V.optionalString)
        |> V.andMap (V.field "requestMethod" V.string)
        |> V.andMap (V.field "requestUrl" V.string)
        |> V.andMap (V.field "requestHeaders" (V.list requestHeaderValidation))
        |> V.andMap (V.field "requestBody" V.optionalString)
        |> V.andMap (V.field "responseListField" V.optionalString)
        |> V.andMap (V.field "responseItemId" V.string)
        |> V.andMap (V.field "responseItemTemplate" V.string)
        |> V.andMap (V.field "responseItemUrl" V.string)
        |> V.andMap (V.field "annotations" validateAnnotations)


validateIntegrationId : List Integration -> String -> Validation FormError String
validateIntegrationId integrations uuid =
    let
        existingUuids =
            List.filter (.uuid >> (/=) uuid) integrations
                |> List.map .id
    in
    V.string
        |> V.andThen
            (\s _ ->
                if List.member s existingUuids then
                    Err <| Error.value (CustomError IntegrationIdAlreadyUsed)

                else
                    Ok s
            )


requestHeaderValidation : Validation FormError ( String, String )
requestHeaderValidation =
    V.map2 Tuple.pair
        (V.field "header" V.string)
        (V.field "value" V.string)


integrationFormInitials : Integration -> List ( String, Field.Field )
integrationFormInitials integration =
    [ ( "id", Field.string integration.id )
    , ( "name", Field.string integration.name )
    , ( "logo", Field.string integration.logo )
    , ( "requestMethod", Field.string integration.requestMethod )
    , ( "requestUrl", Field.string integration.requestUrl )
    , ( "requestHeaders"
      , Field.list
            (List.map
                (\h ->
                    Field.group
                        [ ( "header", Field.string <| Tuple.first h )
                        , ( "value", Field.string <| Tuple.second h )
                        ]
                )
                (Dict.toList integration.requestHeaders)
            )
      )
    , ( "requestBody", Field.string integration.requestBody )
    , ( "responseListField", Field.string integration.responseListField )
    , ( "responseItemId", Field.string integration.responseItemId )
    , ( "responseItemTemplate", Field.string integration.responseItemTemplate )
    , ( "responseItemUrl", Field.string integration.responseItemUrl )
    , ( "annotations", annotationsField integration.annotations )
    ]


updateIntegrationWithForm : Integration -> IntegrationForm -> Integration
updateIntegrationWithForm integration integrationForm =
    { integration
        | id = integrationForm.id
        , name = integrationForm.name
        , logo = integrationForm.logo
        , requestMethod = integrationForm.requestMethod
        , requestUrl = integrationForm.requestUrl
        , requestHeaders = Dict.fromList integrationForm.requestHeaders
        , requestBody = integrationForm.requestBody
        , responseListField = integrationForm.responseListField
        , responseItemId = integrationForm.responseItemId
        , responseItemTemplate = integrationForm.responseItemTemplate
        , responseItemUrl = integrationForm.responseItemUrl
        , annotations = Dict.fromList integrationForm.annotations
    }



{- Chapter -}


initChapterForm : Chapter -> Form FormError ChapterForm
initChapterForm =
    chapterFormInitials >> initForm chapterFormValidation


chapterFormValidation : Validation FormError ChapterForm
chapterFormValidation =
    V.succeed ChapterForm
        |> V.andMap (V.field "title" V.string)
        |> V.andMap (V.field "text" V.maybeString)
        |> V.andMap (V.field "annotations" validateAnnotations)


chapterFormInitials : Chapter -> List ( String, Field.Field )
chapterFormInitials chapter =
    [ ( "title", Field.string chapter.title )
    , ( "text", Field.maybeString chapter.text )
    , ( "annotations", annotationsField chapter.annotations )
    ]


updateChapterWithForm : Chapter -> ChapterForm -> Chapter
updateChapterWithForm chapter chapterForm =
    { chapter
        | title = chapterForm.title
        , text = chapterForm.text
        , annotations = Dict.fromList chapterForm.annotations
    }



{- Question -}


initQuestionForm : List Integration -> Question -> Form FormError QuestionForm
initQuestionForm integrations =
    questionFormInitials >> initForm (questionFormValidation integrations)


questionFormValidation : List Integration -> Validation FormError QuestionForm
questionFormValidation integrations =
    V.succeed QuestionForm
        |> V.andMap (V.field "questionType" V.string |> V.andThen (validateQuestion integrations))


validateQuestion : List Integration -> String -> Validation FormError QuestionFormType
validateQuestion integrations questionType =
    case questionType of
        "OptionsQuestion" ->
            V.succeed OptionsQuestionFormData
                |> V.andMap (V.field "title" V.string)
                |> V.andMap (V.field "text" V.maybeString)
                |> V.andMap (V.field "requiredLevel" V.maybeString)
                |> V.andMap (V.field "annotations" validateAnnotations)
                |> V.map OptionsQuestionForm

        "ListQuestion" ->
            V.succeed ListQuestionFormData
                |> V.andMap (V.field "title" V.string)
                |> V.andMap (V.field "text" V.maybeString)
                |> V.andMap (V.field "requiredLevel" V.maybeString)
                |> V.andMap (V.field "annotations" validateAnnotations)
                |> V.map ListQuestionForm

        "ValueQuestion" ->
            V.succeed ValueQuestionFormData
                |> V.andMap (V.field "title" V.string)
                |> V.andMap (V.field "text" V.maybeString)
                |> V.andMap (V.field "requiredLevel" V.maybeString)
                |> V.andMap (V.field "valueType" validateValueType)
                |> V.andMap (V.field "annotations" validateAnnotations)
                |> V.map ValueQuestionForm

        "IntegrationQuestion" ->
            V.succeed IntegrationQuestionFormData
                |> V.andMap (V.field "title" V.string)
                |> V.andMap (V.field "text" V.maybeString)
                |> V.andMap (V.field "requiredLevel" V.maybeString)
                |> V.andMap (V.field "integrationUuid" V.string)
                |> V.andMap (V.field "integrationUuid" V.string |> V.andThen (validateIntegrationProps integrations))
                |> V.andMap (V.field "annotations" validateAnnotations)
                |> V.map IntegrationQuestionForm

        "MultiChoiceQuestion" ->
            V.succeed MultiChoiceQuestionFormData
                |> V.andMap (V.field "title" V.string)
                |> V.andMap (V.field "text" V.maybeString)
                |> V.andMap (V.field "requiredLevel" V.maybeString)
                |> V.andMap (V.field "annotations" validateAnnotations)
                |> V.map MultiChoiceQuestionForm

        _ ->
            V.fail <| Error.value InvalidString


validateIntegrationProps : List Integration -> String -> Validation FormError (Dict String String)
validateIntegrationProps integrations integration =
    let
        props =
            List.find (\i -> i.uuid == integration) integrations
                |> Maybe.map .props
                |> Maybe.withDefault []

        fold prop acc =
            V.andThen
                (\value ->
                    V.map (\dict -> Dict.insert prop value dict) acc
                )
                (V.field ("props-" ++ prop) V.optionalString)
    in
    List.foldl fold (V.succeed Dict.empty) props


validateValueType : Validation FormError QuestionValueType
validateValueType =
    V.string
        |> V.andThen
            (\valueType ->
                case valueType of
                    "StringValue" ->
                        V.succeed StringQuestionValueType

                    "DateValue" ->
                        V.succeed DateQuestionValueType

                    "NumberValue" ->
                        V.succeed NumberQuestionValueType

                    "TextValue" ->
                        V.succeed TextQuestionValueType

                    _ ->
                        V.fail <| Error.value InvalidString
            )


questionFormInitials : Question -> List ( String, Field.Field )
questionFormInitials question =
    let
        questionType =
            case question of
                OptionsQuestion _ _ ->
                    "OptionsQuestion"

                ListQuestion _ _ ->
                    "ListQuestion"

                ValueQuestion _ _ ->
                    "ValueQuestion"

                IntegrationQuestion _ _ ->
                    "IntegrationQuestion"

                MultiChoiceQuestion _ _ ->
                    "MultiChoiceQuestion"

        props =
            case question of
                IntegrationQuestion _ integrationQuestionData ->
                    Dict.toList integrationQuestionData.props
                        |> List.map (\( prop, value ) -> ( "props-" ++ prop, Field.string value ))

                _ ->
                    []
    in
    [ ( "questionType", Field.string questionType )
    , ( "title", Field.string <| Question.getTitle question )
    , ( "text", Field.maybeString <| Question.getText question )
    , ( "requiredLevel", Field.maybeString <| Question.getRequiredPhaseUuid question )
    , ( "valueType", Field.string <| valueTypeToString <| Maybe.withDefault StringQuestionValueType <| Question.getValueType question )
    , ( "integrationUuid", Field.maybeString <| Question.getIntegrationUuid question )
    , ( "annotations", annotationsField <| Question.getAnnotations question )
    ]
        ++ props


updateQuestionWithForm : Question -> QuestionForm -> Question
updateQuestionWithForm question questionForm =
    case questionForm.question of
        OptionsQuestionForm formData ->
            OptionsQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredPhaseUuid = formData.requiredPhase
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                , annotations = Dict.fromList formData.annotations
                }
                { answerUuids = Question.getAnswerUuids question
                }

        ListQuestionForm formData ->
            ListQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredPhaseUuid = formData.requiredPhase
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                , annotations = Dict.fromList formData.annotations
                }
                { itemTemplateQuestionUuids = Question.getItemQuestionUuids question
                }

        ValueQuestionForm formData ->
            ValueQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredPhaseUuid = formData.requiredPhase
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                , annotations = Dict.fromList formData.annotations
                }
                { valueType = formData.valueType
                }

        IntegrationQuestionForm formData ->
            IntegrationQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredPhaseUuid = formData.requiredPhase
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                , annotations = Dict.fromList formData.annotations
                }
                { integrationUuid = formData.integrationUuid
                , props = formData.props
                }

        MultiChoiceQuestionForm formData ->
            MultiChoiceQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredPhaseUuid = formData.requiredPhase
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                , annotations = Dict.fromList formData.annotations
                }
                { choiceUuids = Question.getChoiceUuids question
                }


questionTypeOptions : AppState -> List ( String, String )
questionTypeOptions appState =
    [ ( "OptionsQuestion", lg "questionType.options" appState )
    , ( "ListQuestion", lg "questionType.list" appState )
    , ( "ValueQuestion", lg "questionType.value" appState )
    , ( "IntegrationQuestion", lg "questionType.integration" appState )
    , ( "MultiChoiceQuestion", lg "questionType.multiChoice" appState )
    ]


valueTypeToString : QuestionValueType -> String
valueTypeToString valueType =
    case valueType of
        StringQuestionValueType ->
            "StringValue"

        DateQuestionValueType ->
            "DateValue"

        NumberQuestionValueType ->
            "NumberValue"

        TextQuestionValueType ->
            "TextValue"


questionValueTypeOptions : AppState -> List ( String, String )
questionValueTypeOptions appState =
    [ ( "StringValue", lg "questionValueType.string" appState )
    , ( "DateValue", lg "questionValueType.date" appState )
    , ( "NumberValue", lg "questionValueType.number" appState )
    , ( "TextValue", lg "questionValueType.text" appState )
    ]


isOptionsQuestionForm : Form FormError QuestionForm -> Bool
isOptionsQuestionForm =
    let
        detectForm questionForm =
            case questionForm of
                OptionsQuestionForm _ ->
                    True

                _ ->
                    False
    in
    isFormType detectForm


isListQuestionForm : Form FormError QuestionForm -> Bool
isListQuestionForm =
    let
        detectForm questionForm =
            case questionForm of
                ListQuestionForm _ ->
                    True

                _ ->
                    False
    in
    isFormType detectForm


isMultiChoiceQuestionForm : Form FormError QuestionForm -> Bool
isMultiChoiceQuestionForm =
    let
        detectForm questionForm =
            case questionForm of
                MultiChoiceQuestionForm _ ->
                    True

                _ ->
                    False
    in
    isFormType detectForm


isFormType : (QuestionFormType -> Bool) -> Form FormError QuestionForm -> Bool
isFormType detectForm form =
    Form.getOutput form
        |> Maybe.map (.question >> detectForm)
        |> Maybe.withDefault False



{- Answer -}


initAnswerForm : List Metric -> Answer -> Form FormError AnswerForm
initAnswerForm metrics =
    answerFormInitials metrics >> initForm (answerFormValidation metrics)


answerFormValidation : List Metric -> Validation FormError AnswerForm
answerFormValidation metrics =
    V.succeed AnswerForm
        |> V.andMap (V.field "label" V.string)
        |> V.andMap (V.field "advice" V.maybeString)
        |> V.andMap (validateMetricMeasures metrics)
        |> V.andMap (V.field "annotations" validateAnnotations)


validateMetricMeasures : List Metric -> Validation FormError (List MetricMeasureForm)
validateMetricMeasures metrics =
    let
        fold metric acc =
            V.andThen
                (\metricMeasureForm -> V.map (\list -> list ++ [ metricMeasureForm ]) acc)
                (metricMeasureValidation metric.uuid ("metricMeasure-" ++ metric.uuid ++ "-"))
    in
    List.foldl fold (V.succeed []) metrics


metricMeasureValidation : String -> String -> Validation FormError MetricMeasureForm
metricMeasureValidation metricUuid prefix =
    V.succeed (MetricMeasureForm metricUuid)
        |> V.andMap (V.field (prefix ++ "enabled") V.bool)
        |> V.andMap (V.field (prefix ++ "enabled") V.bool |> V.andThen (validateMetricMeasureValues prefix))


validateMetricMeasureValues : String -> Bool -> Validation FormError (Maybe MetricMeasureValues)
validateMetricMeasureValues prefix enabled =
    if enabled then
        V.succeed MetricMeasureValues
            |> V.andMap (V.field (prefix ++ "weight") validateMeasureValue)
            |> V.andMap (V.field (prefix ++ "measure") validateMeasureValue)
            |> V.map Just

    else
        V.succeed Nothing


validateMeasureValue : Validation e Float
validateMeasureValue =
    V.float
        |> V.andThen (V.minFloat 0)
        |> V.andThen (V.maxFloat 1)


answerFormInitials : List Metric -> Answer -> List ( String, Field.Field )
answerFormInitials metrics answer =
    let
        metricToFormField metric =
            metricMeasureFormInitials ("metricMeasure-" ++ metric.uuid ++ "-") answer.metricMeasures metric

        metricMeasureFields =
            List.foldr (++) [] (List.map metricToFormField metrics)
    in
    [ ( "label", Field.string answer.label )
    , ( "advice", Field.maybeString answer.advice )
    , ( "annotations", annotationsField answer.annotations )
    ]
        ++ metricMeasureFields


metricMeasureFormInitials : String -> List MetricMeasure -> Metric -> List ( String, Field.Field )
metricMeasureFormInitials prefix metricMeasures metric =
    case List.find (.metricUuid >> (==) metric.uuid) metricMeasures of
        Just metricMeasure ->
            [ ( prefix ++ "enabled", Field.bool True )
            , ( prefix ++ "metricUuid", Field.string metric.uuid )
            , ( prefix ++ "weight", Field.string (fromFloat metricMeasure.weight) )
            , ( prefix ++ "measure", Field.string (fromFloat metricMeasure.measure) )
            ]

        Nothing ->
            [ ( prefix ++ "enabled", Field.bool False )
            , ( prefix ++ "metricUuid", Field.string metric.uuid )
            , ( prefix ++ "weight", Field.string (fromFloat 1) )
            , ( prefix ++ "measure", Field.string (fromFloat 1) )
            ]


updateAnswerWithForm : Answer -> AnswerForm -> Answer
updateAnswerWithForm answer answerForm =
    { answer
        | label = answerForm.label
        , advice = answerForm.advice
        , metricMeasures = getMetricMeasures answerForm
        , annotations = Dict.fromList answerForm.annotations
    }


getMetricMeasures : AnswerForm -> List MetricMeasure
getMetricMeasures answerForm =
    answerForm.metricMeasures
        |> List.filter .enabled
        |> List.map metricMeasureFormToMetricMeasure


metricMeasureFormToMetricMeasure : MetricMeasureForm -> MetricMeasure
metricMeasureFormToMetricMeasure form =
    { metricUuid = form.metricUuid
    , measure = form.values |> Maybe.map .measure |> Maybe.withDefault 0
    , weight = form.values |> Maybe.map .weight |> Maybe.withDefault 0
    }



{- Choice -}


initChoiceForm : Choice -> Form FormError ChoiceForm
initChoiceForm =
    choiceFormInitials >> initForm choiceFormValidation


choiceFormValidation : Validation FormError ChoiceForm
choiceFormValidation =
    V.succeed ChoiceForm
        |> V.andMap (V.field "label" V.string)
        |> V.andMap (V.field "annotations" validateAnnotations)


choiceFormInitials : Choice -> List ( String, Field.Field )
choiceFormInitials choice =
    [ ( "label", Field.string choice.label )
    , ( "annotations", annotationsField choice.annotations )
    ]


updateChoiceWithForm : Choice -> ChoiceForm -> Choice
updateChoiceWithForm choice choiceForm =
    { choice
        | label = choiceForm.label
        , annotations = Dict.fromList choiceForm.annotations
    }



{- Reference -}


initReferenceForm : Reference -> Form FormError ReferenceForm
initReferenceForm =
    referenceFormInitials >> initForm referenceFormValidation


referenceFormValidation : Validation FormError ReferenceForm
referenceFormValidation =
    V.succeed ReferenceForm
        |> V.andMap (V.field "referenceType" V.string |> V.andThen validateReference)


validateReference : String -> Validation FormError ReferenceFormType
validateReference referenceType =
    case referenceType of
        "ResourcePageReference" ->
            V.succeed ResourcePageReferenceFormType
                |> V.andMap (V.field "shortUuid" V.string)
                |> V.andMap (V.field "annotations" validateAnnotations)

        "URLReference" ->
            V.succeed URLReferenceFormType
                |> V.andMap (V.field "url" V.string)
                |> V.andMap (V.field "label" V.string)
                |> V.andMap (V.field "annotations" validateAnnotations)

        "CrossReference" ->
            V.succeed CrossReferenceFormType
                |> V.andMap (V.field "targetUuid" V.uuidString)
                |> V.andMap (V.field "description" V.string)
                |> V.andMap (V.field "annotations" validateAnnotations)

        _ ->
            V.fail <| Error.value InvalidString


referenceFormInitials : Reference -> List ( String, Field.Field )
referenceFormInitials reference =
    case reference of
        ResourcePageReference data ->
            [ ( "referenceType", Field.string "ResourcePageReference" )
            , ( "shortUuid", Field.string data.shortUuid )
            , ( "annotations", annotationsField data.annotations )
            ]

        URLReference data ->
            [ ( "referenceType", Field.string "URLReference" )
            , ( "url", Field.string data.url )
            , ( "label", Field.string data.label )
            , ( "annotations", annotationsField data.annotations )
            ]

        CrossReference data ->
            [ ( "referenceType", Field.string "CrossReference" )
            , ( "targetUuid", Field.string data.targetUuid )
            , ( "description", Field.string data.description )
            , ( "annotations", annotationsField data.annotations )
            ]


updateReferenceWithForm : Reference -> ReferenceForm -> Reference
updateReferenceWithForm reference referenceForm =
    case referenceForm.reference of
        ResourcePageReferenceFormType shortUuid annotations ->
            ResourcePageReference
                { uuid = Reference.getUuid reference
                , shortUuid = shortUuid
                , annotations = Dict.fromList annotations
                }

        URLReferenceFormType url label annotations ->
            URLReference
                { uuid = Reference.getUuid reference
                , url = url
                , label = label
                , annotations = Dict.fromList annotations
                }

        CrossReferenceFormType targetUuid description annotations ->
            CrossReference
                { uuid = Reference.getUuid reference
                , targetUuid = targetUuid
                , description = description
                , annotations = Dict.fromList annotations
                }


referenceTypeOptions : AppState -> List ( String, String )
referenceTypeOptions appState =
    [ ( "ResourcePageReference", lg "referenceType.resourcePage" appState )
    , ( "URLReference", lg "referenceType.url" appState )
    ]



{- Expert -}


initExpertForm : Expert -> Form FormError ExpertForm
initExpertForm =
    expertFormInitials >> initForm expertFormValidation


expertFormValidation : Validation FormError ExpertForm
expertFormValidation =
    V.succeed ExpertForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "annotations" validateAnnotations)


expertFormInitials : Expert -> List ( String, Field.Field )
expertFormInitials expert =
    [ ( "name", Field.string expert.name )
    , ( "email", Field.string expert.email )
    , ( "annotations", annotationsField expert.annotations )
    ]


updateExpertWithForm : Expert -> ExpertForm -> Expert
updateExpertWithForm expert expertForm =
    { expert
        | name = expertForm.name
        , email = expertForm.email
        , annotations = Dict.fromList expertForm.annotations
    }



{- Common -}


validateAnnotations : Validation FormError (List ( String, String ))
validateAnnotations =
    V.list <|
        V.map2 Tuple.pair
            (V.field "key" V.string)
            (V.field "value" V.string)


annotationsField : Dict String String -> Field
annotationsField annotations =
    Field.list
        (List.map
            (\h ->
                Field.group
                    [ ( "key", Field.string <| Tuple.first h )
                    , ( "value", Field.string <| Tuple.second h )
                    ]
            )
            (Dict.toList annotations)
        )
