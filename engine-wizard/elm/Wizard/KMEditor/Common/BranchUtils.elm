module Wizard.KMEditor.Common.BranchUtils exposing (lastVersion)

import List.Extra as List
import Version exposing (Version)


lastVersion :
    { a
        | kmId : String
        , organizationId : String
        , previousPackageId : Maybe String
    }
    -> Maybe Version
lastVersion branch =
    let
        getVersion parent =
            let
                parts =
                    String.split ":" parent

                samePackage =
                    List.getAt 1 parts
                        |> Maybe.map ((==) branch.kmId)
                        |> Maybe.withDefault False

                sameOrganization =
                    List.getAt 0 parts
                        |> Maybe.map ((==) branch.organizationId)
                        |> Maybe.withDefault False
            in
            if sameOrganization && samePackage then
                List.getAt 2 parts
                    |> Maybe.andThen Version.fromString

            else
                Nothing
    in
    Maybe.andThen getVersion branch.previousPackageId
