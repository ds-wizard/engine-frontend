module Common.Setters exposing
    ( setBookReference
    , setKnowledgeModels
    , setLevels
    , setMetrics
    , setMigration
    , setPackage
    , setPackages
    , setPulling
    , setQuestionnaire
    , setQuestionnaireDetail
    , setQuestionnaires
    , setUsers
    )


setBookReference : a -> { b | bookReference : a } -> { b | bookReference : a }
setBookReference value record =
    { record | bookReference = value }


setKnowledgeModels : a -> { b | knowledgeModels : a } -> { b | knowledgeModels : a }
setKnowledgeModels value record =
    { record | knowledgeModels = value }


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


setUsers : a -> { b | users : a } -> { b | users : a }
setUsers value record =
    { record | users = value }
