module Wizard.Projects.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Shared.Utils exposing (flip)
import Url.Parser exposing (..)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "projects" appState
    in
    [ map (wrapRoute << CreateRoute) (s moduleRoot </> s (lr "projects.create" appState) <?> Query.string (lr "projects.create.selected" appState))
    , map (wrapRoute << CreateMigrationRoute) (s moduleRoot </> s (lr "projects.createMigration" appState) </> uuid)
    , map (wrapRoute << flip DetailRoute PlanDetailRoute.Questionnaire) (s moduleRoot </> uuid)
    , map (wrapRoute << flip DetailRoute PlanDetailRoute.Preview) (s moduleRoot </> uuid </> s "preview")
    , map (wrapRoute << flip DetailRoute PlanDetailRoute.TODOs) (s moduleRoot </> uuid </> s "todos")
    , map (wrapRoute << flip DetailRoute PlanDetailRoute.Metrics) (s moduleRoot </> uuid </> s "metrics")
    , map (detailDocumentsRoute wrapRoute) (s moduleRoot </> uuid </> s "documents" <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
    , map (wrapRoute << flip DetailRoute PlanDetailRoute.NewDocument) (s moduleRoot </> uuid </> s "documents" </> s "new")
    , map (wrapRoute << flip DetailRoute PlanDetailRoute.Settings) (s moduleRoot </> uuid </> s "settings")
    , map (PaginationQueryString.wrapRoute (wrapRoute << IndexRoute) (Just "name")) (s moduleRoot <?> Query.int "page" <?> Query.string "q" <?> Query.string "sort")
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "projects.migration" appState) </> uuid)
    ]


detailDocumentsRoute : (Route -> a) -> Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
detailDocumentsRoute wrapRoute questionnaireUuid =
    PaginationQueryString.wrapRoute (wrapRoute << DetailRoute questionnaireUuid << PlanDetailRoute.Documents) (Just "createdAt,desc")


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "projects" appState
    in
    case route of
        CreateRoute selected ->
            case selected of
                Just id ->
                    [ moduleRoot, lr "projects.create" appState, "?" ++ lr "projects.create.selected" appState ++ "=" ++ id ]

                Nothing ->
                    [ moduleRoot, lr "projects.create" appState ]

        CreateMigrationRoute uuid ->
            [ moduleRoot, lr "projects.createMigration" appState, Uuid.toString uuid ]

        DetailRoute uuid subroute ->
            case subroute of
                PlanDetailRoute.Questionnaire ->
                    [ moduleRoot, Uuid.toString uuid ]

                PlanDetailRoute.Preview ->
                    [ moduleRoot, Uuid.toString uuid, "preview" ]

                PlanDetailRoute.TODOs ->
                    [ moduleRoot, Uuid.toString uuid, "todos" ]

                PlanDetailRoute.Metrics ->
                    [ moduleRoot, Uuid.toString uuid, "metrics" ]

                PlanDetailRoute.Documents paginationQueryString ->
                    [ moduleRoot, Uuid.toString uuid, "documents" ++ PaginationQueryString.toUrl paginationQueryString ]

                PlanDetailRoute.NewDocument ->
                    [ moduleRoot, Uuid.toString uuid, "documents", "new" ]

                PlanDetailRoute.Settings ->
                    [ moduleRoot, Uuid.toString uuid, "settings" ]

        IndexRoute paginationQueryString ->
            [ moduleRoot ++ PaginationQueryString.toUrl paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "projects.migration" appState, Uuid.toString uuid ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        DetailRoute _ _ ->
            True

        _ ->
            Perm.hasPerm session Perm.questionnaire
