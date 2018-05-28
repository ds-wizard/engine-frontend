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
    | DSPlanner DSPlanner.Routing.Route
    | KMEditorCreate
    | KMEditorEditor String
    | KMEditorIndex
    | KMEditorMigration String
    | KMEditorPublish String
    | KMPackages KMPackages.Routing.Route
    | Organization
    | Public Public.Routing.Route
    | Users Users.Routing.Route
    | NotAllowed
    | NotFound
    | DataManagementPlans


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
                   , map KMEditorCreate (s "km-editor" </> s "create")
                   , map KMEditorEditor (s "km-editor" </> s "edit" </> string)
                   , map KMEditorIndex (s "km-editor")
                   , map KMEditorMigration (s "km-editor" </> s "migration" </> string)
                   , map KMEditorPublish (s "km-editor" </> s "publish" </> string)
                   , map Organization (s "organization")
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

        DSPlanner route ->
            DSPlanner.Routing.isAllowed route maybeJwt

        KMEditorCreate ->
            hasPerm maybeJwt Perm.knowledgeModel

        KMEditorEditor uuid ->
            hasPerm maybeJwt Perm.knowledgeModel

        KMEditorIndex ->
            hasPerm maybeJwt Perm.knowledgeModel

        KMEditorMigration uuid ->
            hasPerm maybeJwt Perm.knowledgeModelUpgrade

        KMEditorPublish uuid ->
            hasPerm maybeJwt Perm.knowledgeModelPublish

        KMPackages route ->
            KMPackages.Routing.isAllowed route maybeJwt

        Organization ->
            hasPerm maybeJwt Perm.organization

        Public _ ->
            True

        Users route ->
            Users.Routing.isAllowed route maybeJwt

        NotFound ->
            True

        DataManagementPlans ->
            hasPerm maybeJwt Perm.dataManagementPlan

        _ ->
            False


toUrl : Route -> String
toUrl route =
    let
        parts =
            case route of
                Welcome ->
                    [ "welcome" ]

                DSPlanner route ->
                    DSPlanner.Routing.toUrl route

                KMEditorCreate ->
                    [ "km-editor", "create" ]

                KMEditorEditor uuid ->
                    [ "km-editor", "edit", uuid ]

                KMEditorIndex ->
                    [ "km-editor" ]

                KMEditorMigration uuid ->
                    [ "km-editor", "migration", uuid ]

                KMEditorPublish uuid ->
                    [ "km-editor", "publish", uuid ]

                KMPackages route ->
                    KMPackages.Routing.toUrl route

                Organization ->
                    [ "organization" ]

                Public route ->
                    Public.Routing.toUrl route

                Users route ->
                    Users.Routing.toUrl route

                DataManagementPlans ->
                    [ "data-management-plans" ]

                _ ->
                    []
    in
    "/"
        ++ String.join "/" parts
        |> String.split "/?"
        |> String.join "?"


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
