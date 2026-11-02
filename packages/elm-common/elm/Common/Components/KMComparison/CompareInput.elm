module Common.Components.KMComparison.CompareInput exposing (CompareInput, versionOf)

{-| What to compare: two knowledge model packages, one version of each, and the
tags to keep.

A version is the uuid of one row of the package's own `versions`, not a
`Version` — it is what the preview endpoint asks for, and the package is carried
alongside it so the header can name what is being compared.

Empty tags mean every question, which is what the preview endpoint does with an
empty list.

-}

import Common.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import List.Extra as List
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias CompareInput =
    { leftPackage : KnowledgeModelPackageDetail
    , rightPackage : KnowledgeModelPackageDetail
    , leftVersion : Uuid
    , rightVersion : Uuid
    , leftTags : List String
    , rightTags : List String
    }


{-| The version a side is showing: the one its uuid names, or the package's own
when the uuid names none of them.
-}
versionOf : KnowledgeModelPackageDetail -> Uuid -> Version
versionOf package versionUuid =
    List.find (\version -> version.uuid == versionUuid) package.versions
        |> Maybe.map .version
        |> Maybe.withDefault package.version
