module Shared.Setters exposing
    ( setBookReference
    , setBranches
    , setDocuments
    , setKnowledgeModel
    , setLevels
    , setMetrics
    , setMigration
    , setPackage
    , setPackages
    , setPulling
    , setQuestionnaire
    , setQuestionnaireDetail
    , setQuestionnaireModel
    , setQuestionnaires
    , setSelected
    , setTemplate
    , setTemplates
    , setToken
    , setUser
    , setUsers
    )


setBookReference : a -> { b | bookReference : a } -> { b | bookReference : a }
setBookReference value record =
    { record | bookReference = value }


setBranches : a -> { b | branches : a } -> { b | branches : a }
setBranches value record =
    { record | branches = value }


setDocuments : a -> { b | documents : a } -> { b | documents : a }
setDocuments value record =
    { record | documents = value }


setKnowledgeModel : a -> { b | knowledgeModel : a } -> { b | knowledgeModel : a }
setKnowledgeModel value record =
    { record | knowledgeModel = value }


setLevels : a -> { b | levels : a } -> { b | levels : a }
setLevels value record =
    { record | levels = value }


setMigration : a -> { b | migration : a } -> { b | migration : a }
setMigration value record =
    { record | migration = value }


setMetrics : a -> { b | metrics : a } -> { b | metrics : a }
setMetrics value record =
    { record | metrics = value }


setPackage : a -> { b | package : a } -> { b | package : a }
setPackage value record =
    { record | package = value }


setPackages : a -> { b | packages : a } -> { b | packages : a }
setPackages value record =
    { record | packages = value }


setPulling : a -> { b | pulling : a } -> { b | pulling : a }
setPulling value record =
    { record | pulling = value }


setQuestionnaire : a -> { b | questionnaire : a } -> { b | questionnaire : a }
setQuestionnaire value record =
    { record | questionnaire = value }


setQuestionnaireDetail : a -> { b | questionnaireDetail : a } -> { b | questionnaireDetail : a }
setQuestionnaireDetail value record =
    { record | questionnaireDetail = value }


setQuestionnaireModel : a -> { b | questionnaireModel : a } -> { b | questionnaireModel : a }
setQuestionnaireModel value record =
    { record | questionnaireModel = value }


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


setToken : a -> { b | token : a } -> { b | token : a }
setToken value record =
    { record | token = value }


setUser : a -> { b | user : a } -> { b | user : a }
setUser value record =
    { record | user = value }


setUsers : a -> { b | users : a } -> { b | users : a }
setUsers value record =
    { record | users = value }
