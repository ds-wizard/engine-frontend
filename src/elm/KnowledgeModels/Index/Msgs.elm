module KnowledgeModels.Index.Msgs exposing (Msg(..))

import Jwt
import KnowledgeModels.Common.Models exposing (Package)


type Msg
    = GetPackagesCompleted (Result Jwt.JwtError (List Package))
    | ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result Jwt.JwtError String)
