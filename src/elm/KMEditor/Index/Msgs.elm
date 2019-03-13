module KMEditor.Index.Msgs exposing (Msg(..))

import Form
import Jwt
import KMEditor.Common.Models exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)


type Msg
    = GetKnowledgeModelsCompleted (Result Jwt.JwtError (List KnowledgeModel))
    | ShowHideDeleteKnowledgeModal (Maybe KnowledgeModel)
    | DeleteKnowledgeModel
    | DeleteKnowledgeModelCompleted (Result Jwt.JwtError String)
    | PostMigrationCompleted (Result Jwt.JwtError String)
    | ShowHideUpgradeModal (Maybe KnowledgeModel)
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | UpgradeFormMsg Form.Msg
    | DeleteMigration String
    | DeleteMigrationCompleted (Result Jwt.JwtError String)
