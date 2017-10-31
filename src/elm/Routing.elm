module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


type Route
    = Index
    | Organization
    | UserManagement
    | KnowledgeModels
    | KnowledgeModelsEditor
    | KnowledgeModelsCreate
    | Wizzards
    | DataManagementPlans
    | Login
    | NotFound


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Index top
        , map Organization (s "organization")
        , map UserManagement (s "user-management")
        , map KnowledgeModelsCreate (s "knowledge-models" </> s "create")
        , map KnowledgeModelsEditor (s "knowledge-models" </> s "edit")
        , map KnowledgeModels (s "knowledge-models")
        , map Wizzards (s "wizzards")
        , map DataManagementPlans (s "data-management-plans")
        , map Login (s "login")
        ]


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
