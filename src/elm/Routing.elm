module Routing exposing (..)

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import DSPlanner.Routing
import KMPackages.Routing
import Navigation exposing (Location)
import Public.Routing
import UrlParser exposing (..)
import Users.Routing


type Route
    = Welcome
    | Organization
    | KMEditor
    | KMEditorEditor String
    | KMEditorCreate
    | KMEditorPublish String
    | KMEditorMigration String
    | KMPackages KMPackages.Routing.Route
    | DSPlanner DSPlanner.Routing.Route
    | DataManagementPlans
    | NotFound
    | NotAllowed
    | Public Public.Routing.Route
    | Users Users.Routing.Route


matchers : Parser (Route -> a) a
matchers =
    let
        parsers =
            []
                ++ DSPlanner.Routing.parses DSPlanner
                ++ KMPackages.Routing.parsers KMPackages
                ++ Public.Routing.parsers Public
                ++ Users.Routing.parses Users
                ++ [ map Welcome (s "welcome")
                   , map Organization (s "organization")
                   , map KMEditorCreate (s "km-editor" </> s "create")
                   , map KMEditorEditor (s "km-editor" </> s "edit" </> string)
                   , map KMEditorPublish (s "km-editor" </> s "publish" </> string)
                   , map KMEditorMigration (s "km-editor" </> s "migration" </> string)
                   , map KMEditor (s "km-editor")
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

        KMEditorCreate ->
            hasPerm maybeJwt Perm.knowledgeModel

        KMEditorEditor uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        KMEditorPublish uuid ->
            hasPerm maybeJwt Perm.knowledgeModelPublish

        KMEditorMigration uuid ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        KMEditor ->
            hasPerm maybeJwt Perm.knowledgeModel

        KMPackages route ->
            KMPackages.Routing.isAllowed route maybeJwt

        DSPlanner route ->
            DSPlanner.Routing.isAllowed route maybeJwt

        DataManagementPlans ->
            hasPerm maybeJwt Perm.dataManagementPlan

        Public _ ->
            True

        Users route ->
            Users.Routing.isAllowed route maybeJwt

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

                KMEditorCreate ->
                    [ "km-editor", "create" ]

                KMEditorEditor uuid ->
                    [ "km-editor", "edit", uuid ]

                KMEditorPublish uuid ->
                    [ "km-editor", "publish", uuid ]

                KMEditorMigration uuid ->
                    [ "km-editor", "migration", uuid ]

                KMEditor ->
                    [ "km-editor" ]

                KMPackages route ->
                    KMPackages.Routing.toUrl route

                DSPlanner route ->
                    DSPlanner.Routing.toUrl route

                DataManagementPlans ->
                    [ "data-management-plans" ]

                Public route ->
                    Public.Routing.toUrl route

                Users route ->
                    Users.Routing.toUrl route

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
