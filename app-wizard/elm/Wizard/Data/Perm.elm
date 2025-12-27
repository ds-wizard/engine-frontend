module Wizard.Data.Perm exposing
    ( dev
    , documentTemplates
    , hasPerm
    , knowledgeModel
    , knowledgeModelPublish
    , knowledgeModelUpgrade
    , locale
    , packageManagementRead
    , packageManagementWrite
    , project
    , projectAction
    , projectFile
    , projectImporter
    , projectTemplate
    , settings
    , submission
    , tenants
    , userManagement
    )

import Maybe.Extra as Maybe


hasPerm : Maybe { a | permissions : List String } -> String -> Bool
hasPerm mbUserInfo perm =
    List.member perm (Maybe.unwrap [] .permissions mbUserInfo)


tenants : String
tenants =
    "TENANT_PERM"


dev : String
dev =
    "DEV_PERM"


knowledgeModel : String
knowledgeModel =
    "KM_PERM"


knowledgeModelPublish : String
knowledgeModelPublish =
    "KM_PUBLISH_PERM"


knowledgeModelUpgrade : String
knowledgeModelUpgrade =
    "KM_UPGRADE_PERM"


locale : String
locale =
    "LOC_PERM"


packageManagementRead : String
packageManagementRead =
    "PM_READ_PERM"


packageManagementWrite : String
packageManagementWrite =
    "PM_WRITE_PERM"


project : String
project =
    "PRJ_PERM"


projectTemplate : String
projectTemplate =
    "PRJ_TML_PERM"


projectAction : String
projectAction =
    "PRJ_ACTION_PERM"


projectImporter : String
projectImporter =
    "PRJ_IMPORTER_PERM"


projectFile : String
projectFile =
    "PRJ_FILE_PERM"


settings : String
settings =
    "CFG_PERM"


submission : String
submission =
    "SUBM_PERM"


documentTemplates : String
documentTemplates =
    "DOC_TML_WRITE_PERM"


userManagement : String
userManagement =
    "UM_PERM"
