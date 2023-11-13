module Shared.Setters exposing
    ( setApiKey
    , setApiKeys
    , setAssets
    , setBookReference
    , setDropdownState
    , setFiles
    , setFormatUuid
    , setKnowledgeModel
    , setKnowledgeModelString
    , setLocale
    , setMigration
    , setPackage
    , setPackages
    , setPlans
    , setPulling
    , setQuestionnaire
    , setQuestionnaireImporter
    , setQuestionnaireUuid
    , setQuestionnaires
    , setSelected
    , setTemplate
    , setTemplates
    , setTenant
    , setTokens
    , setUsage
    )


setApiKey : a -> { b | apiKey : a } -> { b | apiKey : a }
setApiKey value record =
    { record | apiKey = value }


setApiKeys : a -> { b | apiKeys : a } -> { b | apiKeys : a }
setApiKeys value record =
    { record | apiKeys = value }


setAssets : a -> { b | assets : a } -> { b | assets : a }
setAssets value record =
    { record | assets = value }


setBookReference : a -> { b | bookReference : a } -> { b | bookReference : a }
setBookReference value record =
    { record | bookReference = value }


setDropdownState : a -> { b | dropdownState : a } -> { b | dropdownState : a }
setDropdownState value record =
    { record | dropdownState = value }


setFiles : a -> { b | files : a } -> { b | files : a }
setFiles value record =
    { record | files = value }


setFormatUuid : a -> { b | formatUuid : a } -> { b | formatUuid : a }
setFormatUuid value record =
    { record | formatUuid = value }


setKnowledgeModel : a -> { b | knowledgeModel : a } -> { b | knowledgeModel : a }
setKnowledgeModel value record =
    { record | knowledgeModel = value }


setKnowledgeModelString : a -> { b | knowledgeModelString : a } -> { b | knowledgeModelString : a }
setKnowledgeModelString value record =
    { record | knowledgeModelString = value }


setLocale : a -> { b | locale : a } -> { b | locale : a }
setLocale value record =
    { record | locale = value }


setMigration : a -> { b | migration : a } -> { b | migration : a }
setMigration value record =
    { record | migration = value }


setPackage : a -> { b | package : a } -> { b | package : a }
setPackage value record =
    { record | package = value }


setPackages : a -> { b | packages : a } -> { b | packages : a }
setPackages value record =
    { record | packages = value }


setPlans : a -> { b | plans : a } -> { b | plans : a }
setPlans value record =
    { record | plans = value }


setPulling : a -> { b | pulling : a } -> { b | pulling : a }
setPulling value record =
    { record | pulling = value }


setQuestionnaire : a -> { b | questionnaire : a } -> { b | questionnaire : a }
setQuestionnaire value record =
    { record | questionnaire = value }


setQuestionnaireUuid : a -> { b | questionnaireUuid : a } -> { b | questionnaireUuid : a }
setQuestionnaireUuid value record =
    { record | questionnaireUuid = value }


setQuestionnaireImporter : a -> { b | questionnaireImporter : a } -> { b | questionnaireImporter : a }
setQuestionnaireImporter value record =
    { record | questionnaireImporter = value }


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


setTenant : a -> { b | tenant : a } -> { b | tenant : a }
setTenant value record =
    { record | tenant = value }


setTokens : a -> { b | tokens : a } -> { b | tokens : a }
setTokens value record =
    { record | tokens = value }


setUsage : a -> { b | usage : a } -> { b | usage : a }
setUsage value record =
    { record | usage = value }
