module Wizard.Documents.Routing exposing (..)

import Shared.Locale exposing (lr)
import Url.Parser exposing (..)
import Url.Parser.Query as Query
import Wizard.Auth.Permission as Perm exposing (hasPerm)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Common.Pagination.PaginationQueryString as PaginationQueryString
import Wizard.Documents.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "documents" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "documents.create" appState) <?> Query.string (lr "documents.create.selected" appState))
    , map (indexRoute wrapRoute) (s moduleRoot <?> Query.string (lr "documents.index.questionnaireUuid" appState) <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
    ]


indexRoute : (Route -> a) -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> a
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
                    [ moduleRoot, lr "documents.create" appState, "?" ++ lr "questionnaires.create.selected" appState ++ "=" ++ uuid ]

                Nothing ->
                    [ moduleRoot, lr "documents.create" appState ]

        IndexRoute questionnaireUuid paginationQueryString ->
            let
                queryString =
                    PaginationQueryString.toUrlWith
                        [ ( lr "documents.index.questionnaireUuid" appState, Maybe.withDefault "" questionnaireUuid )
                        ]
                        paginationQueryString
            in
            if String.isEmpty queryString then
                [ moduleRoot ]

            else
                [ moduleRoot, queryString ]


isAllowed : Route -> Maybe JwtToken -> Bool
isAllowed _ maybeJwt =
    hasPerm maybeJwt Perm.dataManagementPlan
