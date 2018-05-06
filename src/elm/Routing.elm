module Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Navigation exposing (Location)
import Public.Routing
import Questionnaires.Routing
import UrlParser exposing (..)
import UserManagement.Routing


type Route
    = Welcome
    | Organization
    | KnowledgeModels
    | KnowledgeModelsEditor String
    | KnowledgeModelsCreate
    | KnowledgeModelsPublish String
    | KnowledgeModelsMigration String
    | PackageManagement
    | PackageManagementDetail String String
    | PackageManagementImport
    | Questionnaires Questionnaires.Routing.Route
    | DataManagementPlans
    | NotFound
    | NotAllowed
    | Public Public.Routing.Route
    | UserManagement UserManagement.Routing.Route


matchers : Parser (Route -> a) a
matchers =
    let
        parsers =
            []
                ++ Public.Routing.parsers Public
                ++ Questionnaires.Routing.parses Questionnaires
                ++ UserManagement.Routing.parses UserManagement
                ++ [ map Welcome (s "welcome")
                   , map Organization (s "organization")
                   , map KnowledgeModelsCreate (s "knowledge-models" </> s "create")
                   , map KnowledgeModelsEditor (s "knowledge-models" </> s "edit" </> string)
                   , map KnowledgeModelsPublish (s "knowledge-models" </> s "publish" </> string)
                   , map KnowledgeModelsMigration (s "knowledge-models" </> s "migration" </> string)
                   , map KnowledgeModels (s "knowledge-models")
                   , map PackageManagement (s "package-management")
                   , map PackageManagementDetail (s "package-management" </> s "package" </> string </> string)
                   , map PackageManagementImport (s "package-management" </> s "import")
                   , map DataManagementPlans (s "data-management-plans")
                   ]
    in
    oneOf parsers


routeIfAllowed : Maybe JwtToken -> Route -> Route
routeIfAllowed maybeJwt route =
    if isAllowed route maybeJwt then
        route
    else
        NotAllowed


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed route maybeJwt =
    case route of
        Welcome ->
            True

        Organization ->
            hasPerm maybeJwt Perm.organization

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

        Questionnaires route ->
            Questionnaires.Routing.isAllowed route maybeJwt

        DataManagementPlans ->
            hasPerm maybeJwt Perm.dataManagementPlan

        Public _ ->
            True

        UserManagement route ->
            UserManagement.Routing.isAllowed route maybeJwt

        NotFound ->
            True

        _ ->
            False


toUrl : Route -> String
toUrl route =
    let
        parts =
            case route of
                Welcome ->
                    [ "welcome" ]

                Organization ->
                    [ "organization" ]

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

                Questionnaires route ->
                    Questionnaires.Routing.toUrl route

                DataManagementPlans ->
                    [ "data-management-plans" ]

                Public route ->
                    Public.Routing.toUrl route

                UserManagement route ->
                    UserManagement.Routing.toUrl route

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


homeRoute : Route
homeRoute =
    Public Public.Routing.Login


loginRoute : Route
loginRoute =
    Public Public.Routing.Login


signupRoute : Route
signupRoute =
    Public Public.Routing.Signup
