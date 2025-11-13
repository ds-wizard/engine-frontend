module Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUtils exposing (lastVersion)

import List.Extra as List
import Version exposing (Version)
import Wizard.Data.AppState exposing (AppState)


lastVersion :
    AppState
    ->
        { a
            | kmId : String
            , previousPackageId : Maybe String
        }
    -> Maybe Version
lastVersion appState kmEditor =
    let
        getVersion parent =
            let
                parts =
                    String.split ":" parent

                samePackage =
                    List.getAt 1 parts
                        |> Maybe.map ((==) kmEditor.kmId)
                        |> Maybe.withDefault False

                sameOrganization =
                    List.getAt 0 parts
                        |> Maybe.map ((==) appState.config.organization.organizationId)
                        |> Maybe.withDefault False
            in
            if sameOrganization && samePackage then
                List.getAt 2 parts
                    |> Maybe.andThen Version.fromString

            else
                Nothing
    in
    Maybe.andThen getVersion kmEditor.previousPackageId
