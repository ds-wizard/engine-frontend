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
import Common.Features exposing (Features)
import KMEditor.Routing
import KnowledgeModels.Routing
import Public.Routing
import Questionnaires.Routing
import Url exposing (Url)
import Url.Parser exposing (..)
import Users.Routing


type Route
    = Welcome
    | KMEditor KMEditor.Routing.Route
    | KnowledgeModels KnowledgeModels.Routing.Route
    | Organization
    | Public Public.Routing.Route
    | Questionnaires Questionnaires.Routing.Route
    | Users Users.Routing.Route
    | NotAllowed
    | NotFound


matchers : Features -> Parser (Route -> a) a
matchers features =
    let
        parsers =
            []
                ++ Questionnaires.Routing.parses Questionnaires
                ++ KMEditor.Routing.parsers KMEditor
                ++ KnowledgeModels.Routing.parsers KnowledgeModels
                ++ Public.Routing.parsers features Public
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

        Questionnaires dsPlannerRoute ->
            Questionnaires.Routing.isAllowed dsPlannerRoute maybeJwt

        KMEditor kmEditorRoute ->
            KMEditor.Routing.isAllowed kmEditorRoute maybeJwt

        KnowledgeModels kmPackagesRoute ->
            KnowledgeModels.Routing.isAllowed kmPackagesRoute maybeJwt

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

                Questionnaires dsPlannerRoute ->
                    Questionnaires.Routing.toUrl dsPlannerRoute

                KMEditor kmEditorRoute ->
                    KMEditor.Routing.toUrl kmEditorRoute

                KnowledgeModels kmPackagesRoute ->
                    KnowledgeModels.Routing.toUrl kmPackagesRoute

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


parseLocation : Features -> Url -> Route
parseLocation features url =
    case Url.Parser.parse (matchers features) url of
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
