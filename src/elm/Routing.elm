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
    | UserManagementDelete String
    | KnowledgeModels
    | KnowledgeModelsEditor
    | KnowledgeModelsCreate
    | Wizzards
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
        , map UserManagementDelete (s "user-management" </> s "delete" </> string)
        , map KnowledgeModelsCreate (s "knowledge-models" </> s "create")
        , map KnowledgeModelsEditor (s "knowledge-models" </> s "edit")
        , map KnowledgeModels (s "knowledge-models")
        , map Wizzards (s "wizzards")
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

        UserManagementDelete uuid ->
            hasPerm maybeJwt Perm.userManagement

        KnowledgeModelsCreate ->
            hasPerm maybeJwt Perm.knowledgeModel

        KnowledgeModelsEditor ->
            hasPerm maybeJwt Perm.knowledgeModel

        KnowledgeModels ->
            hasPerm maybeJwt Perm.knowledgeModel

        Wizzards ->
            hasPerm maybeJwt Perm.wizzard

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

                UserManagementDelete uuid ->
                    [ "user-management", "delete", uuid ]

                KnowledgeModelsCreate ->
                    [ "knowledge-models", "create" ]

                KnowledgeModelsEditor ->
                    [ "knowledge-models", "edit" ]

                KnowledgeModels ->
                    [ "knowledge-models" ]

                Wizzards ->
                    [ "wizzards" ]

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
