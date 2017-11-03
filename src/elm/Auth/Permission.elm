module Auth.Permission exposing (..)

import Auth.Models exposing (JwtToken)


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


organization : String
organization =
    "ORG_PERM"


knowledgeModel : String
knowledgeModel =
    "KM_PERM"


knowledgeModelUpgrade : String
knowledgeModelUpgrade =
    "KM_UPGRADE_PERM"


knowledgeModelPublish : String
knowledgeModelPublish =
    "KM_PUBLISH_PERM"


packageManagement : String
packageManagement =
    "PM_PERM"


wizzard : String
wizzard =
    "WIZ_PERM"


dataManagementPlan : String
dataManagementPlan =
    "DMP_PERM"
