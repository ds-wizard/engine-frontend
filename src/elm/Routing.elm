module Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Index
    | Organization
    | UserManagement
    | UserManagementCreate
    | UserManagementEdit String
    | KnowledgeModels
    | KnowledgeModelsEditor String
    | KnowledgeModelsCreate
    | KnowledgeModelsPublish String
    | KnowledgeModelsMigration String
    | PackageManagement
    | PackageManagementDetail String String
    | PackageManagementImport
    | Wizards
    | DataManagementPlans
    | Login
    | NotFound
    | NotAllowed


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Index top
        , map Organization (s "organization")
        , map UserManagement (s "user-management")
        , map UserManagementCreate (s "user-management" </> s "create")
        , map UserManagementEdit (s "user-management" </> s "edit" </> string)
        , map KnowledgeModelsCreate (s "knowledge-models" </> s "create")
        , map KnowledgeModelsEditor (s "knowledge-models" </> s "edit" </> string)
        , map KnowledgeModelsPublish (s "knowledge-models" </> s "publish" </> string)
        , map KnowledgeModelsMigration (s "knowledge-models" </> s "migration" </> string)
        , map KnowledgeModels (s "knowledge-models")
        , map PackageManagement (s "package-management")
        , map PackageManagementDetail (s "package-management" </> s "package" </> string </> string)
        , map PackageManagementImport (s "package-management" </> s "import")
        , map Wizards (s "wizards")
        , map DataManagementPlans (s "data-management-plans")
        , map Login (s "login")
        ]


routeIfAllowed : Maybe JwtToken -> Route -> Route
routeIfAllowed maybeJwt route =
    if isAllowed route maybeJwt then
        route
    else
        NotAllowed


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Index ->
            True

        Organization ->
            hasPerm maybeJwt Perm.organization

        UserManagement ->
            hasPerm maybeJwt Perm.userManagement

        UserManagementCreate ->
            hasPerm maybeJwt Perm.userManagement

        UserManagementEdit uuid ->
            if uuid == "current" then
                True
            else
                hasPerm maybeJwt Perm.userManagement

        KnowledgeModelsCreate ->
            hasPerm maybeJwt Perm.knowledgeModel

        KnowledgeModelsEditor uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        KnowledgeModelsPublish uuid ->
            hasPerm maybeJwt Perm.knowledgeModelPublish

        KnowledgeModelsMigration uuid ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        KnowledgeModels ->
            hasPerm maybeJwt Perm.knowledgeModel

        PackageManagement ->
            hasPerm maybeJwt Perm.packageManagement

        PackageManagementDetail groupId artifactId ->
            hasPerm maybeJwt Perm.packageManagement

        PackageManagementImport ->
            hasPerm maybeJwt Perm.packageManagement

        Wizards ->
            hasPerm maybeJwt Perm.wizard

        DataManagementPlans ->
            hasPerm maybeJwt Perm.dataManagementPlan

        Login ->
            True

        NotFound ->
            True

        _ ->
            False


toUrl : Route -> String
toUrl route =
    let
        parts =
            case route of
                Index ->
                    []

                Organization ->
                    [ "organization" ]

                UserManagement ->
                    [ "user-management" ]

                UserManagementCreate ->
                    [ "user-management", "create" ]

                UserManagementEdit uuid ->
                    [ "user-management", "edit", uuid ]

                KnowledgeModelsCreate ->
                    [ "knowledge-models", "create" ]

                KnowledgeModelsEditor uuid ->
                    [ "knowledge-models", "edit", uuid ]

                KnowledgeModelsPublish uuid ->
                    [ "knowledge-models", "publish", uuid ]

                KnowledgeModelsMigration uuid ->
                    [ "knowledge-models", "migration", uuid ]

                KnowledgeModels ->
                    [ "knowledge-models" ]

                PackageManagement ->
                    [ "package-management" ]

                PackageManagementDetail groupId artifactId ->
                    [ "package-management", "package", groupId, artifactId ]

                PackageManagementImport ->
                    [ "package-management", "import" ]

                Wizards ->
                    [ "wizards" ]

                DataManagementPlans ->
                    [ "data-management-plans" ]

                Login ->
                    [ "login" ]

                _ ->
                    []
    in
    "/" ++ String.join "/" parts


parseLocation : Location -> Route
parseLocation location =
    case UrlParser.parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFound


cmdNavigate : Route -> Cmd msg
cmdNavigate =
    Navigation.newUrl << toUrl
