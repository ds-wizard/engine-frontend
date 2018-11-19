module Routing exposing
    ( Route(..)
    , appRoute
    , cmdNavigate
    , homeRoute
    , isAllowed
    , loginRoute
    , matchers
    , parseLocation
    , questionnaireDemoRoute
    , routeIfAllowed
    , signupRoute
    , toUrl
    )

import Auth.Models exposing (JwtToken)
import Auth.Permission as Perm exposing (hasPerm)
import Browser.Navigation exposing (Key, pushUrl)
import DSPlanner.Routing
import KMEditor.Routing
import KMPackages.Routing
import Public.Routing
import Url exposing (Url)
import Url.Parser exposing (..)
import Users.Routing


type Route
    = Welcome
    | DSPlanner DSPlanner.Routing.Route
    | KMEditor KMEditor.Routing.Route
    | KMPackages KMPackages.Routing.Route
    | Organization
    | Public Public.Routing.Route
    | Users Users.Routing.Route
    | NotAllowed
    | NotFound


matchers : Parser (Route -> a) a
matchers =
    let
        parsers =
            []
                ++ DSPlanner.Routing.parses DSPlanner
                ++ KMEditor.Routing.parsers KMEditor
                ++ KMPackages.Routing.parsers KMPackages
                ++ Public.Routing.parsers Public
                ++ Users.Routing.parses Users
                ++ [ map Welcome (s "welcome")
                   , map Organization (s "organization")
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

        DSPlanner dsPlannerRoute ->
            DSPlanner.Routing.isAllowed dsPlannerRoute maybeJwt

        KMEditor kmEditorRoute ->
            KMEditor.Routing.isAllowed kmEditorRoute maybeJwt

        KMPackages kmPackagesRoute ->
            KMPackages.Routing.isAllowed kmPackagesRoute maybeJwt

        Organization ->
            hasPerm maybeJwt Perm.organization

        Public _ ->
            True

        Users usersRoute ->
            Users.Routing.isAllowed usersRoute maybeJwt

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

                DSPlanner dsPlannerRoute ->
                    DSPlanner.Routing.toUrl dsPlannerRoute

                KMEditor kmEditorRoute ->
                    KMEditor.Routing.toUrl kmEditorRoute

                KMPackages kmPackagesRoute ->
                    KMPackages.Routing.toUrl kmPackagesRoute

                Organization ->
                    [ "organization" ]

                Public publicRoute ->
                    Public.Routing.toUrl publicRoute

                Users usersRoute ->
                    Users.Routing.toUrl usersRoute

                _ ->
                    []
    in
    "/"
        ++ String.join "/" parts
        |> String.split "/?"
        |> String.join "?"


parseLocation : Url -> Route
parseLocation url =
    case Url.Parser.parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFound


cmdNavigate : Key -> Route -> Cmd msg
cmdNavigate key =
    pushUrl key << toUrl


homeRoute : Route
homeRoute =
    Public Public.Routing.Login


loginRoute : Route
loginRoute =
    Public Public.Routing.Login


signupRoute : Route
signupRoute =
    Public Public.Routing.Signup


questionnaireDemoRoute : Route
questionnaireDemoRoute =
    Public Public.Routing.Questionnaire


appRoute : Route
appRoute =
    Welcome
