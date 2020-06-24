module Wizard.Documents.Routing exposing (..)

import Maybe.Extra as Maybe
import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Url.Parser.Query.Extra as Query
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Documents.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "documents" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "documents.create" appState) <?> Query.uuid (lr "documents.create.selected" appState))
    , map (indexRoute wrapRoute) (s moduleRoot <?> Query.uuid (lr "documents.index.questionnaireUuid" appState) <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
    ]


indexRoute : (Route -> a) -> Maybe Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
indexRoute wrapRoute documentUuid =
    PaginationQueryString.wrapRoute (wrapRoute << IndexRoute documentUuid) (Just "name")


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "documents" appState
    in
    case route of
        CreateRoute selected ->
            case selected of
                Just uuid ->
                    [ moduleRoot, lr "documents.create" appState, "?" ++ lr "questionnaires.create.selected" appState ++ "=" ++ Uuid.toString uuid ]

                Nothing ->
                    [ moduleRoot, lr "documents.create" appState ]

        IndexRoute questionnaireUuid paginationQueryString ->
            let
                queryString =
                    PaginationQueryString.toUrlWith
                        [ ( lr "documents.index.questionnaireUuid" appState, Maybe.unwrap "" Uuid.toString questionnaireUuid )
                        ]
                        paginationQueryString
            in
            if String.isEmpty queryString then
                [ moduleRoot ]

            else
                [ moduleRoot, queryString ]


isAllowed : Route -> Session -> Bool
isAllowed _ session =
    Perm.hasPerm session Perm.dataManagementPlan
