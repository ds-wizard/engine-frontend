module Common.Utils.KnowledgeModelUtils exposing (getPackageId, getPackageIdWithoutVersion)

import Version exposing (Version)


getPackageId : { a | id : String, version : Version } -> String
getPackageId kmPackage =
    kmPackage.id ++ ":" ++ Version.toString kmPackage.version


{-| The package's identity without the version: what the knowledge model is,
rather than which release of it. Use it where the version is shown on its own
already, so it isn't said twice.
-}
getPackageIdWithoutVersion : { a | id : String } -> String
getPackageIdWithoutVersion kmPackage =
    kmPackage.id
