module Common.Utils.Setters exposing
    ( setApiKey
    , setApiKeys
    , setAppKeys
    , setAssets
    , setCommentThreads
    , setDashboard
    , setDebouncer
    , setDev
    , setDocumentTemplates
    , setDropdownState
    , setEntity
    , setFiles
    , setFormatUuid
    , setKnowledgeModel
    , setKnowledgeModelEditorUuid
    , setKnowledgeModelPackage
    , setKnowledgeModelPackages
    , setKnowledgeModelString
    , setKnowledgeModels
    , setLocale
    , setMigration
    , setPlans
    , setProjects
    , setPublic
    , setPulling
    , setQuestionnaire
    , setQuestionnaireImporter
    , setQuestionnaireUuid
    , setQuestionnaires
    , setSaml
    , setSeed
    , setSelected
    , setSettings
    , setShouldSendEmail
    , setSynchronizations
    , setTemplate
    , setTemplates
    , setTenant
    , setTenants
    , setTokens
    , setUsage
    , setUsageAdmin
    , setUsageAnalytics
    , setUsageIntegrationHub
    , setUsageWizard
    , setUser
    , setUserGroups
    , setUsers
    , setValueIntegrations
    , setValues
    )


setApiKey : a -> { b | apiKey : a } -> { b | apiKey : a }
setApiKey value record =
    { record | apiKey = value }


setApiKeys : a -> { b | apiKeys : a } -> { b | apiKeys : a }
setApiKeys value record =
    { record | apiKeys = value }


setAppKeys : a -> { b | appKeys : a } -> { b | appKeys : a }
setAppKeys value record =
    { record | appKeys = value }


setAssets : a -> { b | assets : a } -> { b | assets : a }
setAssets value record =
    { record | assets = value }


setCommentThreads : a -> { b | commentThreads : a } -> { b | commentThreads : a }
setCommentThreads value record =
    { record | commentThreads = value }


setDashboard : a -> { b | dashboard : a } -> { b | dashboard : a }
setDashboard value record =
    { record | dashboard = value }


setDebouncer : a -> { b | debouncer : a } -> { b | debouncer : a }
setDebouncer value record =
    { record | debouncer = value }


setDev : a -> { b | dev : a } -> { b | dev : a }
setDev value record =
    { record | dev = value }


setDocumentTemplates : a -> { b | documentTemplates : a } -> { b | documentTemplates : a }
setDocumentTemplates value record =
    { record | documentTemplates = value }


setDropdownState : a -> { b | dropdownState : a } -> { b | dropdownState : a }
setDropdownState value record =
    { record | dropdownState = value }


setEntity : a -> { b | entity : a } -> { b | entity : a }
setEntity value record =
    { record | entity = value }


setFiles : a -> { b | files : a } -> { b | files : a }
setFiles value record =
    { record | files = value }


setFormatUuid : a -> { b | formatUuid : a } -> { b | formatUuid : a }
setFormatUuid value record =
    { record | formatUuid = value }


setKnowledgeModel : a -> { b | knowledgeModel : a } -> { b | knowledgeModel : a }
setKnowledgeModel value record =
    { record | knowledgeModel = value }


setKnowledgeModelEditorUuid : a -> { b | knowledgeModelEditorUuid : a } -> { b | knowledgeModelEditorUuid : a }
setKnowledgeModelEditorUuid value record =
    { record | knowledgeModelEditorUuid = value }


setKnowledgeModelPackage : a -> { b | knowledgeModelPackage : a } -> { b | knowledgeModelPackage : a }
setKnowledgeModelPackage value record =
    { record | knowledgeModelPackage = value }


setKnowledgeModelPackages : a -> { b | knowledgeModelPackages : a } -> { b | knowledgeModelPackages : a }
setKnowledgeModelPackages value record =
    { record | knowledgeModelPackages = value }


setKnowledgeModelString : a -> { b | knowledgeModelString : a } -> { b | knowledgeModelString : a }
setKnowledgeModelString value record =
    { record | knowledgeModelString = value }


setKnowledgeModels : a -> { b | knowledgeModels : a } -> { b | knowledgeModels : a }
setKnowledgeModels value record =
    { record | knowledgeModels = value }


setLocale : a -> { b | locale : a } -> { b | locale : a }
setLocale value record =
    { record | locale = value }


setMigration : a -> { b | migration : a } -> { b | migration : a }
setMigration value record =
    { record | migration = value }


setPlans : v -> { a | plans : v } -> { a | plans : v }
setPlans value record =
    { record | plans = value }


setProjects : v -> { a | projects : v } -> { a | projects : v }
setProjects value record =
    { record | projects = value }


setPublic : v -> { a | public : v } -> { a | public : v }
setPublic value record =
    { record | public = value }


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


setSaml : v -> { a | saml : v } -> { a | saml : v }
setSaml value record =
    { record | saml = value }


setSeed : v -> { a | seed : v } -> { a | seed : v }
setSeed value record =
    { record | seed = value }


setSelected : a -> { b | selected : a } -> { b | selected : a }
setSelected value record =
    { record | selected = value }


setSettings : v -> { a | settings : v } -> { a | settings : v }
setSettings value record =
    { record | settings = value }


setShouldSendEmail : v -> { a | shouldSendEmail : v } -> { a | shouldSendEmail : v }
setShouldSendEmail value record =
    { record | shouldSendEmail = value }


setSynchronizations : v -> { a | synchronizations : v } -> { a | synchronizations : v }
setSynchronizations value record =
    { record | synchronizations = value }


setTemplate : a -> { b | template : a } -> { b | template : a }
setTemplate value record =
    { record | template = value }


setTemplates : a -> { b | templates : a } -> { b | templates : a }
setTemplates value record =
    { record | templates = value }


setTenant : a -> { b | tenant : a } -> { b | tenant : a }
setTenant value record =
    { record | tenant = value }


setTenants : a -> { b | tenants : a } -> { b | tenants : a }
setTenants value record =
    { record | tenants = value }


setTokens : a -> { b | tokens : a } -> { b | tokens : a }
setTokens value record =
    { record | tokens = value }


setUsage : a -> { b | usage : a } -> { b | usage : a }
setUsage value record =
    { record | usage = value }


setUsageAdmin : v -> { a | usageAdmin : v } -> { a | usageAdmin : v }
setUsageAdmin value record =
    { record | usageAdmin = value }


setUsageAnalytics : v -> { a | usageAnalytics : v } -> { a | usageAnalytics : v }
setUsageAnalytics value record =
    { record | usageAnalytics = value }


setUsageIntegrationHub : v -> { a | usageIntegrationHub : v } -> { a | usageIntegrationHub : v }
setUsageIntegrationHub value record =
    { record | usageIntegrationHub = value }


setUsageWizard : v -> { a | usageWizard : v } -> { a | usageWizard : v }
setUsageWizard value record =
    { record | usageWizard = value }


setUser : v -> { a | user : v } -> { a | user : v }
setUser value record =
    { record | user = value }


setUserGroups : v -> { a | userGroups : v } -> { a | userGroups : v }
setUserGroups value record =
    { record | userGroups = value }


setUsers : v -> { a | users : v } -> { a | users : v }
setUsers value record =
    { record | users = value }


setValueIntegrations : v -> { a | valueIntegrations : v } -> { a | valueIntegrations : v }
setValueIntegrations value record =
    { record | valueIntegrations = value }


setValues : v -> { a | values : v } -> { a | values : v }
setValues value record =
    { record | values = value }
