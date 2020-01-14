module Wizard.Questionnaires.Routing exposing
    ( isAllowed
    , parses
    , toUrl
    )

import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Questionnaires.Routes exposing (Route(..))


parses : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parses appState wrapRoute =
    let
        moduleRoot =
            lr "questionnaires" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "questionnaires.create" appState) <?> Query.string (lr "questionnaires.create.selected" appState))
    , map (wrapRoute << CreateMigrationRoute) (s moduleRoot </> s (lr "questionnaires.createMigration" appState) </> string)
    , map (wrapRoute << DetailRoute) (s moduleRoot </> s (lr "questionnaires.detail" appState) </> string)
    , map (wrapRoute << EditRoute) (s moduleRoot </> s (lr "questionnaires.edit" appState) </> string)
    , map (wrapRoute <| IndexRoute) (s moduleRoot)
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "questionnaires.migration" appState) </> string)
    ]


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "questionnaires" appState
    in
    case route of
        CreateRoute selected ->
            case selected of
                Just id ->
                    [ moduleRoot, lr "questionnaires.create" appState, "?" ++ lr "questionnaires.create.selected" appState ++ "=" ++ id ]

                Nothing ->
                    [ moduleRoot, lr "questionnaires.create" appState ]

        CreateMigrationRoute uuid ->
            [ moduleRoot, lr "questionnaires.createMigration" appState, uuid ]

        DetailRoute uuid ->
            [ moduleRoot, lr "questionnaires.detail" appState, uuid ]

        EditRoute uuid ->
            [ moduleRoot, lr "questionnaires.edit" appState, uuid ]

        IndexRoute ->
            [ moduleRoot ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "questionnaires.migration" appState, uuid ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed _ maybeJwt =
    hasPerm maybeJwt Perm.questionnaire
