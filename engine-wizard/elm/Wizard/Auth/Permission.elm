module Wizard.Auth.Permission exposing
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

import Wizard.Common.JwtToken exposing (JwtToken)


hasPerm : Maybe JwtToken -> String -> Bool
hasPerm maybeJwt perm =
    case maybeJwt of
        Just jwt ->
            List.any ((==) perm) jwt.permissions

        Nothing ->
            False


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
