module Wizard.Questionnaires.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString
import Wizard.Questionnaires.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "questionnaires" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "questionnaires.create" appState) <?> Query.string (lr "questionnaires.create.selected" appState))
    , map (wrapRoute << CreateMigrationRoute) (s moduleRoot </> s (lr "questionnaires.createMigration" appState) </> string)
    , map (wrapRoute << DetailRoute) (s moduleRoot </> s (lr "questionnaires.detail" appState) </> string)
    , map (wrapRoute << EditRoute) (s moduleRoot </> s (lr "questionnaires.edit" appState) </> string)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (s moduleRoot <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
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

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "questionnaires.migration" appState, uuid ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed _ maybeJwt =
    hasPerm maybeJwt Perm.questionnaire
