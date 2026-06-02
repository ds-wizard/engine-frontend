module Wizard.Components.KMComparison.CompareInput exposing (CompareInput)

import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)


type alias CompareInput =
    { leftPackage : KnowledgeModelPackageDetail
    , rightPackage : KnowledgeModelPackageDetail
    , leftVersion : Uuid
    , rightVersion : Uuid
    , leftTags : List String
    , rightTags : List String
    }
