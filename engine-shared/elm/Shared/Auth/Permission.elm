module Shared.Auth.Permission exposing
    ( dev
    , documentTemplates
    , hasPerm
    , knowledgeModel
    , knowledgeModelPublish
    , knowledgeModelUpgrade
    , locale
    , packageManagementRead
    , packageManagementWrite
    , questionnaire
    , questionnaireImporter
    , questionnaireTemplate
    , settings
    , submission
    , tenants
    , userManagement
    )

import Maybe.Extra as Maybe
import Shared.Auth.Session exposing (Session)


hasPerm : Session -> String -> Bool
hasPerm session perm =
    List.member perm (Maybe.unwrap [] .permissions session.user)


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


questionnaire : String
questionnaire =
    "QTN_PERM"


questionnaireTemplate : String
questionnaireTemplate =
    "QTN_TML_PERM"


questionnaireImporter : String
questionnaireImporter =
    "QTN_IMPORTER_PERM"


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
