module Wizard.Projects.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Shared.Auth.Permission as Perm
import Shared.Data.PaginationQueryFilters.FilterOperator as FilterOperator
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Locale exposing (lr)
import Shared.Utils exposing (flip)
import Shared.Utils.UrlUtils exposing (queryParamsToString)
import Url.Parser exposing ((</>), (<?>), Parser, map, s, string)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Url.Parser.Query.Extra as Query
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes exposing (Route(..), indexRouteIsTemplateFilterId, indexRoutePackagesFilterId, indexRouteProjectTagsFilterId, indexRouteUsersFilterId)


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "projects" appState

        -- Project documents
        newDocumentRoute projectUuid mbEventUuid =
            wrapRoute <| DetailRoute projectUuid <| ProjectDetailRoute.NewDocument mbEventUuid

        -- Project create
        projectCreateRoute selectedProjectTemplate selectedKnowledgeModel =
            wrapRoute (CreateRoute selectedProjectTemplate selectedKnowledgeModel)

        -- Project index
        wrappedIndexRoute pqs mbTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp =
            wrapRoute <| IndexRoute pqs mbTemplate mbUser mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp

        indexRouteParser =
            PaginationQueryString.parser7 (s moduleRoot)
                (Query.string indexRouteIsTemplateFilterId)
                (Query.string indexRouteUsersFilterId)
                (FilterOperator.queryParser indexRouteUsersFilterId)
                (Query.string indexRouteProjectTagsFilterId)
                (FilterOperator.queryParser indexRouteProjectTagsFilterId)
                (Query.string indexRoutePackagesFilterId)
                (FilterOperator.queryParser indexRoutePackagesFilterId)

        projectDetailQuestionnaire projectUuid mbQuestionPath mbCommentThreadUuid =
            wrapRoute <| DetailRoute projectUuid (ProjectDetailRoute.Questionnaire mbQuestionPath mbCommentThreadUuid)

        -- Project Import
        projectImportRoute uuid string =
            wrapRoute <| ImportRoute uuid string

        documentDownloadRoute projectUuid documentUuid =
            wrapRoute <| DocumentDownloadRoute projectUuid documentUuid

        fileDownloadRoute projectUuid documentUuid =
            wrapRoute <| FileDownloadRoute projectUuid documentUuid
    in
    [ map projectCreateRoute (s moduleRoot </> s (lr "projects.create" appState) <?> Query.uuid (lr "projects.create.selectedProjectTemplate" appState) <?> Query.string (lr "projects.create.selectedKnowledgeModel" appState))
    , map (wrapRoute << CreateMigrationRoute) (s moduleRoot </> s (lr "projects.createMigration" appState) </> uuid)
    , map projectDetailQuestionnaire (s moduleRoot </> uuid <?> Query.string "questionPath" <?> Query.uuid "commentThreadUuid")
    , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Preview) (s moduleRoot </> uuid </> s "preview")
    , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Metrics) (s moduleRoot </> uuid </> s "metrics")
    , map (detailDocumentsRoute wrapRoute) (PaginationQueryString.parser (s moduleRoot </> uuid </> s "documents"))
    , map newDocumentRoute (s moduleRoot </> uuid </> s "documents" </> s "new" <?> Query.uuid "eventUuid")
    , map (detailFilesRoute wrapRoute) (PaginationQueryString.parser (s moduleRoot </> uuid </> s "files"))
    , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Settings) (s moduleRoot </> uuid </> s "settings")
    , map (PaginationQueryString.wrapRoute7 wrappedIndexRoute (Just "updatedAt,desc")) indexRouteParser
    , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "projects.migration" appState) </> uuid)
    , map projectImportRoute (s moduleRoot </> s "import" </> uuid </> string)
    , map documentDownloadRoute (s moduleRoot </> uuid </> s "documents" </> uuid </> s "download")
    , map fileDownloadRoute (s moduleRoot </> uuid </> s "files" </> uuid </> s "download")
    ]


detailDocumentsRoute : (Route -> a) -> Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
detailDocumentsRoute wrapRoute questionnaireUuid =
    PaginationQueryString.wrapRoute (wrapRoute << DetailRoute questionnaireUuid << ProjectDetailRoute.Documents) (Just "createdAt,desc")


detailFilesRoute : (Route -> a) -> Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
detailFilesRoute wrapRoute questionnaireUuid =
    PaginationQueryString.wrapRoute (wrapRoute << DetailRoute questionnaireUuid << ProjectDetailRoute.Files) (Just "createdAt,desc")


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "projects" appState
    in
    case route of
        CreateRoute selectedProjectTemplate selectedKnowledgeModel ->
            let
                queryString =
                    queryParamsToString
                        [ ( lr "projects.create.selectedProjectTemplate" appState, Maybe.map Uuid.toString selectedProjectTemplate )
                        , ( lr "projects.create.selectedKnowledgeModel" appState, selectedKnowledgeModel )
                        ]
            in
            [ moduleRoot, lr "projects.create" appState ++ queryString ]

        CreateMigrationRoute uuid ->
            [ moduleRoot, lr "projects.createMigration" appState, Uuid.toString uuid ]

        DetailRoute uuid subroute ->
            case subroute of
                ProjectDetailRoute.Questionnaire mbQuestionPath mbCommentThreadUuid ->
                    let
                        queryString =
                            queryParamsToString
                                [ ( "questionPath", mbQuestionPath )
                                , ( "commentThreadUuid", Maybe.map Uuid.toString mbCommentThreadUuid )
                                ]
                    in
                    [ moduleRoot, Uuid.toString uuid ++ queryString ]

                ProjectDetailRoute.Preview ->
                    [ moduleRoot, Uuid.toString uuid, "preview" ]

                ProjectDetailRoute.Metrics ->
                    [ moduleRoot, Uuid.toString uuid, "metrics" ]

                ProjectDetailRoute.Documents paginationQueryString ->
                    [ moduleRoot, Uuid.toString uuid, "documents" ++ PaginationQueryString.toUrl paginationQueryString ]

                ProjectDetailRoute.NewDocument mbEventUuid ->
                    case mbEventUuid of
                        Just eventUuid ->
                            [ moduleRoot, Uuid.toString uuid, "documents", "new", "?eventUuid=" ++ Uuid.toString eventUuid ]

                        Nothing ->
                            [ moduleRoot, Uuid.toString uuid, "documents", "new" ]

                ProjectDetailRoute.Files paginationQueryString ->
                    [ moduleRoot, Uuid.toString uuid, "files" ++ PaginationQueryString.toUrl paginationQueryString ]

                ProjectDetailRoute.Settings ->
                    [ moduleRoot, Uuid.toString uuid, "settings" ]

        IndexRoute paginationQueryString mbIsTemplate mbUserUuid mbUserOp mbProjectTags mbProjectTagsOp mbPackages mbPackagesOp ->
            let
                params =
                    PaginationQueryString.filterParams
                        [ ( indexRouteIsTemplateFilterId, mbIsTemplate )
                        , ( indexRouteUsersFilterId, mbUserUuid )
                        , FilterOperator.toUrlParam indexRouteUsersFilterId mbUserOp
                        , ( indexRouteProjectTagsFilterId, mbProjectTags )
                        , FilterOperator.toUrlParam indexRouteProjectTagsFilterId mbProjectTagsOp
                        , ( indexRoutePackagesFilterId, mbPackages )
                        , FilterOperator.toUrlParam indexRoutePackagesFilterId mbPackagesOp
                        ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "projects.migration" appState, Uuid.toString uuid ]

        ImportRoute uuid importerId ->
            [ moduleRoot, "import", Uuid.toString uuid, importerId ]

        DocumentDownloadRoute projectUuid documentUuid ->
            [ moduleRoot, Uuid.toString projectUuid, "documents", Uuid.toString documentUuid, "download" ]

        FileDownloadRoute projectUuid documentUuid ->
            [ moduleRoot, Uuid.toString projectUuid, "files", Uuid.toString documentUuid, "download" ]


isAllowed : Route -> AppState -> Bool
isAllowed route appState =
    case route of
        DetailRoute _ _ ->
            True

        DocumentDownloadRoute _ _ ->
            True

        FileDownloadRoute _ _ ->
            True

        _ ->
            Perm.hasPerm appState.config.user Perm.questionnaire
