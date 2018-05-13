module KMEditor.Index.Msgs exposing (..)

{-|

@docs Msg

-}

import Form
import Jwt
import KMEditor.Models exposing (KnowledgeModel)
import KMPackages.Common.Models exposing (PackageDetail)


{-| -}
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
