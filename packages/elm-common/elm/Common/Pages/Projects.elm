module Common.Pages.Projects exposing
    ( Model
    , Msg
    , entityName
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Projects: the feature router.

The listing, create, create-migration, the two download links, the detail and
the importer. The knowledge model comparison inside create-migration is still a
dialog saying it is not there yet.

The detail is the one page here that keeps state worth keeping between routes:
its tabs are routes, and it holds a websocket open for as long as the project is
on screen. The model is therefore rebuilt only when the project changes, and the
socket is closed on the way out.

-}

import Common.Api.WebSocket as WebSocket
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Projects.Create as Create
import Common.Pages.Projects.CreateMigration as CreateMigration
import Common.Pages.Projects.Detail as Detail
import Common.Pages.Projects.DocumentDownload as DocumentDownload
import Common.Pages.Projects.FileDownload as FileDownload
import Common.Pages.Projects.Import as Import
import Common.Pages.Projects.Index as Index
import Common.Pages.Projects.Routes as ProjectsRoutes
import Common.Routes as Routes
import Html exposing (Html)
import Random
import Uuid


type alias Model =
    { index : Index.Model
    , create : Create.Model
    , createMigration : CreateMigration.Model
    , detail : Detail.Model
    , documentDownload : DocumentDownload.Model
    , fileDownload : FileDownload.Model
    , import_ : Import.Model
    }


type Msg
    = IndexMsg Index.Msg
    | CreateMsg Create.Msg
    | CreateMigrationMsg CreateMigration.Msg
    | DetailMsg Detail.Msg
    | DocumentDownloadMsg DocumentDownload.Msg
    | FileDownloadMsg FileDownload.Msg
    | ImportMsg Import.Msg


initialModel : AppState -> Model
initialModel appState =
    { index = Index.initialModel
    , create = Create.initialModel appState Nothing Nothing
    , createMigration = CreateMigration.initialModel appState Uuid.nil
    , detail = Detail.initialModel appState Uuid.nil
    , documentDownload = DocumentDownload.initialModel Uuid.nil Uuid.nil
    , fileDownload = FileDownload.initialModel Uuid.nil Uuid.nil
    , import_ = Import.initialModel appState Uuid.nil ""
    }


{-| The project detail holds a websocket open while it is on screen, so leaving
it closes the socket and forgets the project; staying on the same project and only changing tab keeps both.
-}
leaveDetail : AppState -> Model -> ProjectsRoutes.Route -> ( Model, Cmd Msg )
leaveDetail appState model route =
    let
        stayingOnSameProject =
            case route of
                ProjectsRoutes.DetailRoute uuid _ ->
                    uuid == model.detail.uuid

                _ ->
                    False
    in
    if stayingOnSameProject || model.detail.uuid == Uuid.nil then
        ( model, Cmd.none )

    else
        ( { model | detail = Detail.initialModel appState Uuid.nil }
        , WebSocket.close model.detail.websocket
        )


init : AppState -> Model -> ProjectsRoutes.Route -> ( Model, Cmd Msg )
init appState previousModel route =
    let
        ( model, leaveCmd ) =
            leaveDetail appState previousModel route

        withLeaveCmd ( nextModel, cmd ) =
            ( nextModel, Cmd.batch [ leaveCmd, cmd ] )
    in
    withLeaveCmd <|
        case route of
            ProjectsRoutes.IndexRoute pqs isTemplate users usersOp userGroups userGroupsOp projectTags projectTagsOp packages packagesOp ->
                let
                    ( index, cmd ) =
                        Index.init appState
                            { paginationQueryString = pqs
                            , isTemplate = isTemplate
                            , users = users
                            , usersOp = usersOp
                            , userGroups = userGroups
                            , userGroupsOp = userGroupsOp
                            , projectTags = projectTags
                            , projectTagsOp = projectTagsOp
                            , packages = packages
                            , packagesOp = packagesOp
                            }
                in
                ( { model | index = index }, Cmd.map IndexMsg cmd )

            ProjectsRoutes.CreateRoute selectedProjectTemplate selectedKnowledgeModel ->
                let
                    ( create, cmd ) =
                        Create.init appState selectedProjectTemplate selectedKnowledgeModel
                in
                ( { model | create = create }, Cmd.map CreateMsg cmd )

            ProjectsRoutes.DocumentDownloadRoute projectUuid documentUuid ->
                let
                    ( documentDownload, cmd ) =
                        DocumentDownload.init appState projectUuid documentUuid
                in
                ( { model | documentDownload = documentDownload }, Cmd.map DocumentDownloadMsg cmd )

            ProjectsRoutes.FileDownloadRoute projectUuid fileUuid ->
                let
                    ( fileDownload, cmd ) =
                        FileDownload.init appState projectUuid fileUuid
                in
                ( { model | fileDownload = fileDownload }, Cmd.map FileDownloadMsg cmd )

            ProjectsRoutes.DetailRoute uuid subroute ->
                let
                    ( freshDetail, cmd ) =
                        Detail.init appState uuid subroute

                    -- Another tab of the same project: the project, the people
                    -- online and the open socket stay, and only the request the
                    -- tab needs is sent again.
                    ( detail, reenterCmd ) =
                        if uuid == model.detail.uuid then
                            Detail.reenter subroute model.detail

                        else
                            ( freshDetail, Cmd.none )
                in
                ( { model | detail = detail }, Cmd.map DetailMsg (Cmd.batch [ cmd, reenterCmd ]) )

            ProjectsRoutes.ImportRoute uuid importerUrl ->
                let
                    ( import_, cmd ) =
                        Import.init appState uuid importerUrl
                in
                ( { model | import_ = import_ }, Cmd.map ImportMsg cmd )

            ProjectsRoutes.CreateMigrationRoute uuid ->
                let
                    ( createMigration, cmd ) =
                        CreateMigration.init appState uuid
                in
                ( { model | createMigration = createMigration }, Cmd.map CreateMigrationMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( index, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.index
            in
            ( appState.seed, { model | index = index }, cmd )

        CreateMsg createMsg ->
            let
                ( create, cmd ) =
                    Create.update (UpdateConfig.map CreateMsg cfg) appState createMsg model.create
            in
            ( appState.seed, { model | create = create }, cmd )

        CreateMigrationMsg createMigrationMsg ->
            let
                ( createMigration, cmd ) =
                    CreateMigration.update (UpdateConfig.map CreateMigrationMsg cfg) appState createMigrationMsg model.createMigration
            in
            ( appState.seed, { model | createMigration = createMigration }, cmd )

        DetailMsg detailMsg ->
            let
                ( seed, detail, cmd ) =
                    Detail.update (UpdateConfig.map DetailMsg cfg) appState detailMsg model.detail
            in
            ( seed, { model | detail = detail }, cmd )

        DocumentDownloadMsg documentDownloadMsg ->
            let
                ( documentDownload, cmd ) =
                    DocumentDownload.update (UpdateConfig.map DocumentDownloadMsg cfg) appState documentDownloadMsg model.documentDownload
            in
            ( appState.seed, { model | documentDownload = documentDownload }, cmd )

        FileDownloadMsg fileDownloadMsg ->
            let
                ( fileDownload, cmd ) =
                    FileDownload.update (UpdateConfig.map FileDownloadMsg cfg) appState fileDownloadMsg model.fileDownload
            in
            ( appState.seed, { model | fileDownload = fileDownload }, cmd )

        ImportMsg importMsg ->
            let
                ( seed, import_, cmd ) =
                    Import.update (UpdateConfig.map ImportMsg cfg) appState importMsg model.import_
            in
            ( seed, { model | import_ = import_ }, cmd )


subscriptions : AppState -> ProjectsRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        ProjectsRoutes.IndexRoute _ _ _ _ _ _ _ _ _ _ ->
            Sub.map IndexMsg (Index.subscriptions model.index)

        ProjectsRoutes.CreateRoute _ _ ->
            Sub.map CreateMsg (Create.subscriptions appState model.create)

        ProjectsRoutes.CreateMigrationRoute _ ->
            Sub.map CreateMigrationMsg (CreateMigration.subscriptions appState model.createMigration)

        ProjectsRoutes.DetailRoute _ subroute ->
            Sub.map DetailMsg (Detail.subscriptions appState subroute model.detail)

        ProjectsRoutes.ImportRoute _ _ ->
            Sub.map ImportMsg (Import.subscriptions model.import_)

        _ ->
            Sub.none


view : AppState -> ProjectsRoutes.Route -> Model -> Html Msg
view appState route model =
    case route of
        ProjectsRoutes.IndexRoute _ _ _ _ _ _ _ _ _ _ ->
            Html.map IndexMsg (Index.view appState model.index)

        ProjectsRoutes.CreateRoute _ _ ->
            Html.map CreateMsg (Create.view appState model.create)

        ProjectsRoutes.DocumentDownloadRoute _ _ ->
            DocumentDownload.view appState model.documentDownload

        ProjectsRoutes.FileDownloadRoute _ _ ->
            FileDownload.view appState model.fileDownload

        ProjectsRoutes.DetailRoute _ subroute ->
            Html.map DetailMsg (Detail.view appState subroute model.detail)

        ProjectsRoutes.ImportRoute _ _ ->
            Html.map ImportMsg (Import.view appState model.import_)

        ProjectsRoutes.CreateMigrationRoute _ ->
            Html.map CreateMigrationMsg (CreateMigration.view appState model.createMigration)


{-| `mbNextRoute` is where the user is going, when it is known: the project's
questionnaire keeps them only while its changes are saving and they are leaving
the project, not moving to another of its tabs.
-}
isGuarded : AppState -> ProjectsRoutes.Route -> Maybe Routes.Route -> Model -> Maybe String
isGuarded appState route mbNextRoute model =
    case route of
        ProjectsRoutes.DetailRoute _ subroute ->
            Detail.isGuarded appState subroute mbNextRoute model.detail

        _ ->
            Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> ProjectsRoutes.Route -> Maybe String
entityName model route =
    case route of
        ProjectsRoutes.DetailRoute _ _ ->
            Detail.entityName model.detail

        _ ->
            Nothing
