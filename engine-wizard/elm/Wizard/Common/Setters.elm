module Wizard.Common.Setters exposing
    ( setBookReference
    , setBranches
    , setDocuments
    , setLevels
    , setMetrics
    , setMigration
    , setPackage
    , setPackages
    , setPulling
    , setQuestionnaire
    , setQuestionnaireDetail
    , setQuestionnaires
    , setTemplates
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


setQuestionnaires : a -> { b | questionnaires : a } -> { b | questionnaires : a }
setQuestionnaires value record =
    { record | questionnaires = value }


setTemplates : a -> { b | templates : a } -> { b | templates : a }
setTemplates value record =
    { record | templates = value }


setUsers : a -> { b | users : a } -> { b | users : a }
setUsers value record =
    { record | users = value }
