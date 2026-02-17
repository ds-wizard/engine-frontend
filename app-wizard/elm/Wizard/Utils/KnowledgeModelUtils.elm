module Wizard.Utils.KnowledgeModelUtils exposing (getPackageId)

import Version exposing (Version)


getPackageId : { a | organizationId : String, kmId : String, version : Version } -> String
getPackageId kmPackage =
    kmPackage.organizationId ++ ":" ++ kmPackage.kmId ++ ":" ++ Version.toString kmPackage.version
