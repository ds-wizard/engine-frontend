module Common.Setters exposing
    ( setBookReference
    , setKnowledgeModel
    , setKnowledgeModels
    , setLevels
    , setMigration
    , setPackages
    , setQuestionnaires
    , setUsers
    )


setBookReference : a -> { b | bookReference : a } -> { b | bookReference : a }
setBookReference value record =
    { record | bookReference = value }


setKnowledgeModel : a -> { b | knowledgeModel : a } -> { b | knowledgeModel : a }
setKnowledgeModel value record =
    { record | knowledgeModel = value }


setKnowledgeModels : a -> { b | knowledgeModels : a } -> { b | knowledgeModels : a }
setKnowledgeModels value record =
    { record | knowledgeModels = value }


setLevels : a -> { b | levels : a } -> { b | levels : a }
setLevels value record =
    { record | levels = value }


setMigration : a -> { b | migration : a } -> { b | migration : a }
setMigration value record =
    { record | migration = value }


setPackages : a -> { b | packages : a } -> { b | packages : a }
setPackages value record =
    { record | packages = value }


setQuestionnaires : a -> { b | questionnaires : a } -> { b | questionnaires : a }
setQuestionnaires value record =
    { record | questionnaires = value }


setUsers : a -> { b | users : a } -> { b | users : a }
setUsers value record =
    { record | users = value }
