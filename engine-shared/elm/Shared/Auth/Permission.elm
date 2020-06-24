module Shared.Auth.Permission exposing
    ( dataManagementPlan
    , hasPerm
    , knowledgeModel
    , knowledgeModelPublish
    , knowledgeModelUpgrade
    , packageManagementRead
    , packageManagementWrite
    , questionnaire
    , settings
    , submission
    , userManagement
    )

import Maybe.Extra as Maybe
import Shared.Auth.Session exposing (Session)


hasPerm : Session -> String -> Bool
hasPerm session perm =
    List.any ((==) perm) (Maybe.unwrap [] .permissions session.user)


userManagement : String
userManagement =
    "UM_PERM"


knowledgeModel : String
knowledgeModel =
    "KM_PERM"


knowledgeModelUpgrade : String
knowledgeModelUpgrade =
    "KM_UPGRADE_PERM"


knowledgeModelPublish : String
knowledgeModelPublish =
    "KM_PUBLISH_PERM"


packageManagementWrite : String
packageManagementWrite =
    "PM_WRITE_PERM"


packageManagementRead : String
packageManagementRead =
    "PM_READ_PERM"


questionnaire : String
questionnaire =
    "QTN_PERM"


dataManagementPlan : String
dataManagementPlan =
    "DMP_PERM"


submission : String
submission =
    "SUBM_PERM"


settings : String
settings =
    "CFG_PERM"
