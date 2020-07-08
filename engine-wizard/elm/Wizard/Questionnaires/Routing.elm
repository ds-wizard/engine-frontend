module Wizard.Questionnaires.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "questionnaires" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "questionnaires.create" appState) <?> Query.string (lr "questionnaires.create.selected" appState))
    , map (wrapRoute << CreateMigrationRoute) (s moduleRoot </> s (lr "questionnaires.createMigration" appState) </> uuid)
    , map (wrapRoute << DetailRoute) (s moduleRoot </> s (lr "questionnaires.detail" appState) </> uuid)
    , map (wrapRoute << EditRoute) (s moduleRoot </> s (lr "questionnaires.edit" appState) </> uuid)
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (s moduleRoot <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "questionnaires.migration" appState) </> uuid)
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
            [ moduleRoot, lr "questionnaires.createMigration" appState, Uuid.toString uuid ]

        DetailRoute uuid ->
            [ moduleRoot, lr "questionnaires.detail" appState, Uuid.toString uuid ]

        EditRoute uuid ->
            [ moduleRoot, lr "questionnaires.edit" appState, Uuid.toString uuid ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "questionnaires.migration" appState, Uuid.toString uuid ]


isAllowed : Route -> Session -> Bool
isAllowed _ session =
    Perm.hasPerm session Perm.questionnaire
