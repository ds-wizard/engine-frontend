module Shared.Setters exposing
    ( setApp
    , setBookReference
    , setDropdownState
    , setKnowledgeModel
    , setMigration
    , setPackage
    , setPlans
    , setPulling
    , setQuestionnaire
    , setQuestionnaires
    , setSelected
    , setTemplate
    , setTemplates
    , setUsage
    )


setApp : a -> { b | app : a } -> { b | app : a }
setApp value record =
    { record | app = value }


setBookReference : a -> { b | bookReference : a } -> { b | bookReference : a }
setBookReference value record =
    { record | bookReference = value }


setDropdownState : a -> { b | dropdownState : a } -> { b | dropdownState : a }
setDropdownState value record =
    { record | dropdownState = value }


setKnowledgeModel : a -> { b | knowledgeModel : a } -> { b | knowledgeModel : a }
setKnowledgeModel value record =
    { record | knowledgeModel = value }


setMigration : a -> { b | migration : a } -> { b | migration : a }
setMigration value record =
    { record | migration = value }


setPackage : a -> { b | package : a } -> { b | package : a }
setPackage value record =
    { record | package = value }


setPlans : a -> { b | plans : a } -> { b | plans : a }
setPlans value record =
    { record | plans = value }


setPulling : a -> { b | pulling : a } -> { b | pulling : a }
setPulling value record =
    { record | pulling = value }


setQuestionnaire : a -> { b | questionnaire : a } -> { b | questionnaire : a }
setQuestionnaire value record =
    { record | questionnaire = value }


setQuestionnaires : a -> { b | questionnaires : a } -> { b | questionnaires : a }
setQuestionnaires value record =
    { record | questionnaires = value }


setSelected : a -> { b | selected : a } -> { b | selected : a }
setSelected value record =
    { record | selected = value }


setTemplate : a -> { b | template : a } -> { b | template : a }
setTemplate value record =
    { record | template = value }


setTemplates : a -> { b | templates : a } -> { b | templates : a }
setTemplates value record =
    { record | templates = value }


setUsage : a -> { b | usage : a } -> { b | usage : a }
setUsage value record =
    { record | usage = value }
