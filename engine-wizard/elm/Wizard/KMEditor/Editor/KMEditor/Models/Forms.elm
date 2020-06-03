module Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing
    ( AnswerForm
    , ChapterForm
    , ExpertForm
    , IntegrationForm
    , KnowledgeModelForm
    , MetricMeasureForm
    , QuestionForm
    , QuestionFormType(..)
    , ReferenceForm
    , ReferenceFormType(..)
    , TagForm
    , answerFormValidation
    , chapterFormValidation
    , expertFormValidation
    , formChanged
    , getMetricMesures
    , initAnswerForm
    , initChapterForm
    , initExpertForm
    , initForm
    , initIntegrationForm
    , initKnowledgeModelFrom
    , initQuestionForm
    , initReferenceForm
    , initTagForm
    , integrationFormValidation
    , isListQuestionForm
    , isOptionsQuestionForm
    , knowledgeModelFormValidation
    , metricMeasureValidation
    , questionFormValidation
    , questionTypeOptions
    , questionValueTypeOptions
    , referenceFormValidation
    , referenceTypeOptions
    , tagFormValidation
    , updateAnswerWithForm
    , updateChapterWithForm
    , updateExpertWithForm
    , updateIntegrationWithForm
    , updateKnowledgeModelWithForm
    , updateQuestionWithForm
    , updateReferenceWithForm
    , updateTagWithForm
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import List.Extra as List
import Set
import Shared.Locale exposing (l, lg)
import String exposing (fromFloat, fromInt)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError(..))
import Wizard.Common.Form.Validate as Validate
import Wizard.KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import Wizard.KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.KMEditor.Common.KnowledgeModel.Expert exposing (Expert)
import Wizard.KMEditor.Common.KnowledgeModel.Integration exposing (Integration)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.KMEditor.Common.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Wizard.KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.KMEditor.Common.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Wizard.KMEditor.Common.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Wizard.KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import Wizard.KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)


type alias KnowledgeModelForm =
    { name : String }


type alias TagForm =
    { name : String
    , description : Maybe String
    , color : String
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
    , responseIdField : String
    , responseNameField : String
    , itemUrl : String
    }


type alias ChapterForm =
    { title : String
    , text : Maybe String
    }


type alias QuestionForm =
    { question : QuestionFormType }


type QuestionFormType
    = OptionsQuestionForm OptionsQuestionFormData
    | ListQuestionForm ListQuestionFormData
    | ValueQuestionForm ValueQuestionFormData
    | IntegrationQuestionForm IntegrationQuestionFormData


type alias OptionsQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    }


type alias ListQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    }


type alias ValueQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , valueType : QuestionValueType
    }


type alias IntegrationQuestionFormData =
    { title : String
    , text : Maybe String
    , requiredLevel : Maybe Int
    , integrationUuid : String
    , props : Dict String String
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



{- Tag -}


initTagForm : Tag -> Form CustomFormError TagForm
initTagForm =
    tagFormInitials >> initForm tagFormValidation


tagFormValidation : Validation CustomFormError TagForm
tagFormValidation =
    Validate.map3 TagForm
        (Validate.field "name" Validate.string)
        (Validate.field "description" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
        (Validate.field "color" Validate.string)


tagFormInitials : Tag -> List ( String, Field.Field )
tagFormInitials tag =
    [ ( "name", Field.string tag.name )
    , ( "description", Field.string (tag.description |> Maybe.withDefault "") )
    , ( "color", Field.string tag.color )
    ]


updateTagWithForm : Tag -> TagForm -> Tag
updateTagWithForm tag tagForm =
    { tag
        | name = tagForm.name
        , description = tagForm.description
        , color = tagForm.color
    }



{- Integration -}


initIntegrationForm : List Integration -> String -> Integration -> Form CustomFormError IntegrationForm
initIntegrationForm integrations uuid =
    integrationFormInitials >> initForm (integrationFormValidation integrations uuid)


integrationFormValidation : List Integration -> String -> Validation CustomFormError IntegrationForm
integrationFormValidation integrations uuid =
    Validate.map8 IntegrationForm
        (Validate.field "id" (validateIntegrationId integrations uuid))
        (Validate.field "name" Validate.string)
        (Validate.field "logo" (Validate.oneOf [ Validate.string, Validate.emptyString ]))
        (Validate.field "requestMethod" Validate.string)
        (Validate.field "requestUrl" Validate.string)
        (Validate.field "requestHeaders" (Validate.list requestHeaderValidation))
        (Validate.field "requestBody" (Validate.oneOf [ Validate.string, Validate.emptyString ]))
        (Validate.field "responseListField" (Validate.oneOf [ Validate.emptyString, Validate.string ]))
        |> Validate.andMap (Validate.field "responseIdField" Validate.string)
        |> Validate.andMap (Validate.field "responseNameField" Validate.string)
        |> Validate.andMap (Validate.field "itemUrl" (Validate.oneOf [ Validate.string, Validate.emptyString ]))


validateIntegrationId : List Integration -> String -> Validation CustomFormError String
validateIntegrationId integrations uuid =
    let
        existingUuids =
            List.filter (.uuid >> (/=) uuid) integrations
                |> List.map .id
    in
    Validate.string
        |> Validate.andThen
            (\s v ->
                if List.member s existingUuids then
                    Err <| Error.value (CustomError IntegrationIdAlreadyUsed)

                else
                    Ok s
            )


requestHeaderValidation : Validation CustomFormError ( String, String )
requestHeaderValidation =
    Validate.map2 Tuple.pair
        (Validate.field "header" Validate.string)
        (Validate.field "value" Validate.string)


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
    , ( "responseIdField", Field.string integration.responseIdField )
    , ( "responseNameField", Field.string integration.responseNameField )
    , ( "itemUrl", Field.string integration.itemUrl )
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
        , responseIdField = integrationForm.responseIdField
        , responseNameField = integrationForm.responseNameField
        , itemUrl = integrationForm.itemUrl
    }



{- Chapter -}


initChapterForm : Chapter -> Form CustomFormError ChapterForm
initChapterForm =
    chapterFormInitials >> initForm chapterFormValidation


chapterFormValidation : Validation CustomFormError ChapterForm
chapterFormValidation =
    Validate.map2 ChapterForm
        (Validate.field "title" Validate.string)
        (Validate.field "text" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))


chapterFormInitials : Chapter -> List ( String, Field.Field )
chapterFormInitials chapter =
    [ ( "title", Field.string chapter.title )
    , ( "text", Field.string <| Maybe.withDefault "" chapter.text )
    ]


updateChapterWithForm : Chapter -> ChapterForm -> Chapter
updateChapterWithForm chapter chapterForm =
    { chapter | title = chapterForm.title, text = chapterForm.text }



{- Question -}


initQuestionForm : List Integration -> Question -> Form CustomFormError QuestionForm
initQuestionForm integrations =
    questionFormInitials >> initForm (questionFormValidation integrations)


questionFormValidation : List Integration -> Validation CustomFormError QuestionForm
questionFormValidation integrations =
    Validate.succeed QuestionForm
        |> Validate.andMap (Validate.field "questionType" Validate.string |> Validate.andThen (validateQuestion integrations))


validateQuestion : List Integration -> String -> Validation CustomFormError QuestionFormType
validateQuestion integrations questionType =
    case questionType of
        "OptionsQuestion" ->
            Validate.map3 OptionsQuestionFormData
                (Validate.field "title" Validate.string)
                (Validate.field "text" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
                (Validate.field "requiredLevel" (Validate.maybe Validate.int))
                |> Validate.map OptionsQuestionForm

        "ListQuestion" ->
            Validate.map3 ListQuestionFormData
                (Validate.field "title" Validate.string)
                (Validate.field "text" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
                (Validate.field "requiredLevel" (Validate.maybe Validate.int))
                |> Validate.map ListQuestionForm

        "ValueQuestion" ->
            Validate.map4 ValueQuestionFormData
                (Validate.field "title" Validate.string)
                (Validate.field "text" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
                (Validate.field "requiredLevel" (Validate.maybe Validate.int))
                (Validate.field "valueType" validateValueType)
                |> Validate.map ValueQuestionForm

        "IntegrationQuestion" ->
            Validate.map5 IntegrationQuestionFormData
                (Validate.field "title" Validate.string)
                (Validate.field "text" (Validate.oneOf [ Validate.emptyString |> Validate.map (\_ -> Nothing), Validate.string |> Validate.map Just ]))
                (Validate.field "requiredLevel" (Validate.maybe Validate.int))
                (Validate.field "integrationUuid" Validate.string)
                (Validate.field "integrationUuid" Validate.string |> Validate.andThen (validateIntegrationProps integrations))
                |> Validate.map IntegrationQuestionForm

        _ ->
            Validate.fail <| Error.value InvalidString


validateIntegrationProps : List Integration -> String -> Validation CustomFormError (Dict String String)
validateIntegrationProps integrations integration =
    let
        props =
            List.find (\i -> i.uuid == integration) integrations
                |> Maybe.map .props
                |> Maybe.withDefault []

        fold prop acc =
            Validate.andThen
                (\value ->
                    Validate.map (\dict -> Dict.insert prop value dict) acc
                )
                (Validate.field ("props-" ++ prop) (Validate.oneOf [ Validate.string, Validate.emptyString ]))
    in
    List.foldl fold (Validate.succeed Dict.empty) props


validateValueType : Validation CustomFormError QuestionValueType
validateValueType =
    Validate.string
        |> Validate.andThen
            (\valueType ->
                case valueType of
                    "StringValue" ->
                        Validate.succeed StringQuestionValueType

                    "DateValue" ->
                        Validate.succeed DateQuestionValueType

                    "NumberValue" ->
                        Validate.succeed NumberQuestionValueType

                    "TextValue" ->
                        Validate.succeed TextQuestionValueType

                    _ ->
                        Validate.fail <| Error.value InvalidString
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
    , ( "text", Field.string <| Maybe.withDefault "" <| Question.getText question )
    , ( "requiredLevel", Field.string <| Maybe.withDefault "" <| Maybe.map fromInt <| Question.getRequiredLevel question )
    , ( "valueType", Field.string <| valueTypeToString <| Maybe.withDefault StringQuestionValueType <| Question.getValueType question )
    , ( "integrationUuid", Field.string <| Maybe.withDefault "" <| Question.getIntegrationUuid question )
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
                , requiredLevel = formData.requiredLevel
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                }
                { answerUuids = Question.getAnswerUuids question
                }

        ListQuestionForm formData ->
            ListQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredLevel = formData.requiredLevel
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                }
                { itemTemplateQuestionUuids = Question.getItemQuestionUuids question
                }

        ValueQuestionForm formData ->
            ValueQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredLevel = formData.requiredLevel
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                }
                { valueType = formData.valueType
                }

        IntegrationQuestionForm formData ->
            IntegrationQuestion
                { uuid = Question.getUuid question
                , title = formData.title
                , text = formData.text
                , requiredLevel = formData.requiredLevel
                , tagUuids = Question.getTagUuids question
                , referenceUuids = Question.getReferenceUuids question
                , expertUuids = Question.getExpertUuids question
                }
                { integrationUuid = formData.integrationUuid
                , props = formData.props
                }


questionTypeOptions : AppState -> List ( String, String )
questionTypeOptions appState =
    [ ( "OptionsQuestion", lg "questionType.options" appState )
    , ( "ListQuestion", lg "questionType.list" appState )
    , ( "ValueQuestion", lg "questionType.value" appState )
    , ( "IntegrationQuestion", lg "questionType.integration" appState )
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


isOptionsQuestionForm : Form CustomFormError QuestionForm -> Bool
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


isListQuestionForm : Form CustomFormError QuestionForm -> Bool
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


isFormType : (QuestionFormType -> Bool) -> Form CustomFormError QuestionForm -> Bool
isFormType detectForm form =
    Form.getOutput form
        |> Maybe.map (.question >> detectForm)
        |> Maybe.withDefault False



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
                |> Validate.andMap (Validate.field "targetUuid" Validate.uuid)
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
                { uuid = Reference.getUuid reference
                , shortUuid = shortUuid
                }

        URLReferenceFormType url label ->
            URLReference
                { uuid = Reference.getUuid reference
                , url = url
                , label = label
                }

        CrossReferenceFormType targetUuid description ->
            CrossReference
                { uuid = Reference.getUuid reference
                , targetUuid = targetUuid
                , description = description
                }


referenceTypeOptions : AppState -> List ( String, String )
referenceTypeOptions appState =
    [ ( "ResourcePageReference", lg "referenceType.resourcePage" appState )
    , ( "URLReference", lg "referenceType.url" appState )
    , ( "CrossReference", lg "referenceType.cross" appState )
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
