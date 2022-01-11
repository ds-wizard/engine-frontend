module Shared.Data.Event.EditEventSetters exposing
    ( setAbbreviation
    , setAdvice
    , setAnnotations
    , setAnswerUuids
    , setChapterUuids
    , setChoiceUuids
    , setColor
    , setDescription
    , setEmail
    , setExpertUuids
    , setFollowUpUuids
    , setId
    , setIntegrationUuid
    , setIntegrationUuids
    , setItemTemplateQuestionUuids
    , setLabel
    , setLogo
    , setMetricMeasures
    , setMetricUuids
    , setName
    , setPhaseUuids
    , setProps
    , setQuestionUuids
    , setReferenceUuids
    , setRequestBody
    , setRequestHeaders
    , setRequestMethod
    , setRequestUrl
    , setRequiredPhaseUuid
    , setResponseItemId
    , setResponseItemTemplate
    , setResponseItemUrl
    , setResponseListField
    , setShortUuid
    , setTagUuids
    , setText
    , setTitle
    , setUrl
    , setValueType
    )

import Shared.Data.Event.EventField as EventField exposing (EventField)


setAbbreviation : a -> { b | abbreviation : EventField a } -> { b | abbreviation : EventField a }
setAbbreviation value data =
    { data | abbreviation = EventField.create value True }


setAdvice : a -> { b | advice : EventField a } -> { b | advice : EventField a }
setAdvice value data =
    { data | advice = EventField.create value True }


setAnnotations : a -> { b | annotations : EventField a } -> { b | annotations : EventField a }
setAnnotations value data =
    { data | annotations = EventField.create value True }


setAnswerUuids : a -> { b | answerUuids : EventField a } -> { b | answerUuids : EventField a }
setAnswerUuids value data =
    { data | answerUuids = EventField.create value True }


setChapterUuids : a -> { b | chapterUuids : EventField a } -> { b | chapterUuids : EventField a }
setChapterUuids value data =
    { data | chapterUuids = EventField.create value True }


setChoiceUuids : a -> { b | choiceUuids : EventField a } -> { b | choiceUuids : EventField a }
setChoiceUuids value data =
    { data | choiceUuids = EventField.create value True }


setColor : a -> { b | color : EventField a } -> { b | color : EventField a }
setColor value data =
    { data | color = EventField.create value True }


setDescription : a -> { b | description : EventField a } -> { b | description : EventField a }
setDescription value data =
    { data | description = EventField.create value True }


setEmail : a -> { b | email : EventField a } -> { b | email : EventField a }
setEmail value data =
    { data | email = EventField.create value True }


setExpertUuids : a -> { b | expertUuids : EventField a } -> { b | expertUuids : EventField a }
setExpertUuids value data =
    { data | expertUuids = EventField.create value True }


setFollowUpUuids : a -> { b | followUpUuids : EventField a } -> { b | followUpUuids : EventField a }
setFollowUpUuids value data =
    { data | followUpUuids = EventField.create value True }


setId : a -> { b | id : EventField a } -> { b | id : EventField a }
setId value data =
    { data | id = EventField.create value True }


setIntegrationUuid : a -> { b | integrationUuid : EventField a } -> { b | integrationUuid : EventField a }
setIntegrationUuid value data =
    { data | integrationUuid = EventField.create value True }


setIntegrationUuids : a -> { b | integrationUuids : EventField a } -> { b | integrationUuids : EventField a }
setIntegrationUuids value data =
    { data | integrationUuids = EventField.create value True }


setItemTemplateQuestionUuids : a -> { b | itemTemplateQuestionUuids : EventField a } -> { b | itemTemplateQuestionUuids : EventField a }
setItemTemplateQuestionUuids value data =
    { data | itemTemplateQuestionUuids = EventField.create value True }


setLabel : a -> { b | label : EventField a } -> { b | label : EventField a }
setLabel value data =
    { data | label = EventField.create value True }


setLogo : a -> { b | logo : EventField a } -> { b | logo : EventField a }
setLogo value data =
    { data | logo = EventField.create value True }


setMetricMeasures : a -> { b | metricMeasures : EventField a } -> { b | metricMeasures : EventField a }
setMetricMeasures value data =
    { data | metricMeasures = EventField.create value True }


setMetricUuids : a -> { b | metricUuids : EventField a } -> { b | metricUuids : EventField a }
setMetricUuids value data =
    { data | metricUuids = EventField.create value True }


setName : a -> { b | name : EventField a } -> { b | name : EventField a }
setName value data =
    { data | name = EventField.create value True }


setPhaseUuids : a -> { b | phaseUuids : EventField a } -> { b | phaseUuids : EventField a }
setPhaseUuids value data =
    { data | phaseUuids = EventField.create value True }


setProps : a -> { b | props : EventField a } -> { b | props : EventField a }
setProps value data =
    { data | props = EventField.create value True }


setQuestionUuids : a -> { b | questionUuids : EventField a } -> { b | questionUuids : EventField a }
setQuestionUuids value data =
    { data | questionUuids = EventField.create value True }


setReferenceUuids : a -> { b | referenceUuids : EventField a } -> { b | referenceUuids : EventField a }
setReferenceUuids value data =
    { data | referenceUuids = EventField.create value True }


setRequestBody : a -> { b | requestBody : EventField a } -> { b | requestBody : EventField a }
setRequestBody value data =
    { data | requestBody = EventField.create value True }


setRequestHeaders : a -> { b | requestHeaders : EventField a } -> { b | requestHeaders : EventField a }
setRequestHeaders value data =
    { data | requestHeaders = EventField.create value True }


setRequestMethod : a -> { b | requestMethod : EventField a } -> { b | requestMethod : EventField a }
setRequestMethod value data =
    { data | requestMethod = EventField.create value True }


setRequestUrl : a -> { b | requestUrl : EventField a } -> { b | requestUrl : EventField a }
setRequestUrl value data =
    { data | requestUrl = EventField.create value True }


setRequiredPhaseUuid : a -> { b | requiredPhaseUuid : EventField a } -> { b | requiredPhaseUuid : EventField a }
setRequiredPhaseUuid value data =
    { data | requiredPhaseUuid = EventField.create value True }


setResponseItemId : a -> { b | responseItemId : EventField a } -> { b | responseItemId : EventField a }
setResponseItemId value data =
    { data | responseItemId = EventField.create value True }


setResponseItemTemplate : a -> { b | responseItemTemplate : EventField a } -> { b | responseItemTemplate : EventField a }
setResponseItemTemplate value data =
    { data | responseItemTemplate = EventField.create value True }


setResponseItemUrl : a -> { b | responseItemUrl : EventField a } -> { b | responseItemUrl : EventField a }
setResponseItemUrl value data =
    { data | responseItemUrl = EventField.create value True }


setResponseListField : a -> { b | responseListField : EventField a } -> { b | responseListField : EventField a }
setResponseListField value data =
    { data | responseListField = EventField.create value True }


setShortUuid : a -> { b | shortUuid : EventField a } -> { b | shortUuid : EventField a }
setShortUuid value data =
    { data | shortUuid = EventField.create value True }


setTagUuids : a -> { b | tagUuids : EventField a } -> { b | tagUuids : EventField a }
setTagUuids value data =
    { data | tagUuids = EventField.create value True }


setText : a -> { b | text : EventField a } -> { b | text : EventField a }
setText value data =
    { data | text = EventField.create value True }


setTitle : a -> { b | title : EventField a } -> { b | title : EventField a }
setTitle value data =
    { data | title = EventField.create value True }


setUrl : a -> { b | url : EventField a } -> { b | url : EventField a }
setUrl value data =
    { data | url = EventField.create value True }


setValueType : a -> { b | valueType : EventField a } -> { b | valueType : EventField a }
setValueType value data =
    { data | valueType = EventField.create value True }
