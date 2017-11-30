module KnowledgeModels.Index.Msgs exposing (..)

import Form
import Jwt
import KnowledgeModels.Models exposing (KnowledgeModel)
import PackageManagement.Models exposing (PackageDetail)


type Msg
    = GetKnowledgeModelsCompleted (Result Jwt.JwtError (List KnowledgeModel))
    | ShowHideDeleteKnowledgeModal (Maybe KnowledgeModel)
    | DeleteKnowledgeModel
    | DeleteKnowledgeModelCompleted (Result Jwt.JwtError String)
    | PostMigrationCompleted (Result Jwt.JwtError String)
    | ShowHideUpgradeModal (Maybe KnowledgeModel)
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | UpgradeFormMsg Form.Msg
