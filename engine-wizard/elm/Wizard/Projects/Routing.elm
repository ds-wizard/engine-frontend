module Wizard.Projects.Routing exposing
    ( isAllowed
    , parsers
    , toUrl
    )

import Dict
import Shared.Auth.Permission as Perm
import Shared.Auth.Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Locale exposing (lr)
import Shared.Utils exposing (dictFromMaybeList, flip)
import Url.Parser exposing (..)
import Url.Parser.Extra exposing (uuid)
import Url.Parser.Query as Query
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Create.ProjectCreateRoute as ProjectCreateRoute
import Wizard.Projects.Detail.ProjectDetailRoute as ProjectDetailRoute
import Wizard.Projects.Routes exposing (Route(..))


parsers : AppState -> (Route -> a) -> List (Parser (a -> c) c)
parsers appState wrapRoute =
    let
        moduleRoot =
            lr "projects" appState

        -- Project documents
        newDocumentRoute projectUuid mbEventUuid =
            wrapRoute <| DetailRoute projectUuid <| ProjectDetailRoute.NewDocument mbEventUuid

        -- Project create
        createSegment =
            lr "projects.create" appState

        fromTemplateSegment =
            lr "projects.create.template" appState

        customSegment =
            lr "projects.create.custom" appState

        selectedParam =
            lr "projects.create.selected" appState

        createFromTemplateRoute =
            if QuestionnaireCreation.fromTemplateEnabled appState.config.questionnaire.questionnaireCreation || Perm.hasPerm appState.session Perm.questionnaireTemplate then
                [ map (wrapRoute << CreateRoute << ProjectCreateRoute.TemplateCreateRoute) (s moduleRoot </> s createSegment </> s fromTemplateSegment <?> Query.string selectedParam) ]

            else
                []

        createCustomRoute =
            if QuestionnaireCreation.customEnabled appState.config.questionnaire.questionnaireCreation || Perm.hasPerm appState.session Perm.questionnaireTemplate then
                [ map (wrapRoute << CreateRoute << ProjectCreateRoute.CustomCreateRoute) (s moduleRoot </> s createSegment </> s customSegment <?> Query.string selectedParam) ]

            else
                []

        -- Project index
        wrappedIndexRoute pqs q =
            wrapRoute <| IndexRoute pqs q
    in
    createFromTemplateRoute
        ++ createCustomRoute
        ++ [ map (wrapRoute << CreateMigrationRoute) (s moduleRoot </> s (lr "projects.createMigration" appState) </> uuid)
           , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Questionnaire) (s moduleRoot </> uuid)
           , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Preview) (s moduleRoot </> uuid </> s "preview")
           , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Metrics) (s moduleRoot </> uuid </> s "metrics")
           , map (detailDocumentsRoute wrapRoute) (PaginationQueryString.parser (s moduleRoot </> uuid </> s "documents"))
           , map newDocumentRoute (s moduleRoot </> uuid </> s "documents" </> s "new" <?> Query.string "eventUuid")
           , map (wrapRoute << flip DetailRoute ProjectDetailRoute.Settings) (s moduleRoot </> uuid </> s "settings")
           , map (PaginationQueryString.wrapRoute1 wrappedIndexRoute (Just "updatedAt,desc")) (PaginationQueryString.parser1 (s moduleRoot) (Query.string "isTemplate"))
           , map (wrapRoute << MigrationRoute) (s moduleRoot </> s (lr "projects.migration" appState) </> uuid)
           ]


detailDocumentsRoute : (Route -> a) -> Uuid -> Maybe Int -> Maybe String -> Maybe String -> a
detailDocumentsRoute wrapRoute questionnaireUuid =
    PaginationQueryString.wrapRoute (wrapRoute << DetailRoute questionnaireUuid << ProjectDetailRoute.Documents) (Just "createdAt,desc")


toUrl : AppState -> Route -> List String
toUrl appState route =
    let
        moduleRoot =
            lr "projects" appState
    in
    case route of
        CreateRoute subroute ->
            case subroute of
                ProjectCreateRoute.TemplateCreateRoute mbSelected ->
                    case mbSelected of
                        Just id ->
                            [ moduleRoot, lr "projects.create" appState, lr "projects.create.template" appState, "?" ++ lr "projects.create.selected" appState ++ "=" ++ id ]

                        Nothing ->
                            [ moduleRoot, lr "projects.create" appState, lr "projects.create.template" appState ]

                ProjectCreateRoute.CustomCreateRoute mbSelected ->
                    case mbSelected of
                        Just id ->
                            [ moduleRoot, lr "projects.create" appState, lr "projects.create.custom" appState, "?" ++ lr "projects.create.selected" appState ++ "=" ++ id ]

                        Nothing ->
                            [ moduleRoot, lr "projects.create" appState, lr "projects.create.custom" appState ]

        CreateMigrationRoute uuid ->
            [ moduleRoot, lr "projects.createMigration" appState, Uuid.toString uuid ]

        DetailRoute uuid subroute ->
            case subroute of
                ProjectDetailRoute.Questionnaire ->
                    [ moduleRoot, Uuid.toString uuid ]

                ProjectDetailRoute.Preview ->
                    [ moduleRoot, Uuid.toString uuid, "preview" ]

                ProjectDetailRoute.Metrics ->
                    [ moduleRoot, Uuid.toString uuid, "metrics" ]

                ProjectDetailRoute.Documents paginationQueryString ->
                    [ moduleRoot, Uuid.toString uuid, "documents" ++ PaginationQueryString.toUrl paginationQueryString ]

                ProjectDetailRoute.NewDocument mbEventUuid ->
                    case mbEventUuid of
                        Just eventUuid ->
                            [ moduleRoot, Uuid.toString uuid, "documents", "new", "?eventUuid=" ++ eventUuid ]

                        Nothing ->
                            [ moduleRoot, Uuid.toString uuid, "documents", "new" ]

                ProjectDetailRoute.Settings ->
                    [ moduleRoot, Uuid.toString uuid, "settings" ]

        IndexRoute paginationQueryString mbIsTemplate ->
            let
                params =
                    Dict.toList <| dictFromMaybeList [ ( "isTemplate", mbIsTemplate ) ]
            in
            [ moduleRoot ++ PaginationQueryString.toUrlWith params paginationQueryString ]

        MigrationRoute uuid ->
            [ moduleRoot, lr "projects.migration" appState, Uuid.toString uuid ]


isAllowed : Route -> Session -> Bool
isAllowed route session =
    case route of
        DetailRoute _ _ ->
            True

        _ ->
            Perm.hasPerm session Perm.questionnaire
