module Common.Pages.KMEditor exposing
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

{-| Knowledge Model Editors: the feature router.

The index, create, publish, the editor and its five tabs, and the migration.

-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.KMEditor.Create as Create
import Common.Pages.KMEditor.Editor as Editor
import Common.Pages.KMEditor.Index as Index
import Common.Pages.KMEditor.Migration as Migration
import Common.Pages.KMEditor.Publish as Publish
import Common.Pages.KMEditor.Routes exposing (Route(..))
import Common.Routes as Routes
import Html exposing (Html)
import Random
import Uuid


type alias Model =
    { createModel : Create.Model
    , indexModel : Index.Model
    , publishModel : Publish.Model
    , editorModel : Editor.Model
    , migrationModel : Migration.Model
    }


type Msg
    = CreateMsg Create.Msg
    | IndexMsg Index.Msg
    | PublishMsg Publish.Msg
    | EditorMsg Editor.Msg
    | MigrationMsg Migration.Msg


initialModel : AppState -> Model
initialModel appState =
    { createModel = Create.initialModel appState Nothing Nothing
    , indexModel = Index.initialModel PaginationQueryString.empty
    , publishModel = Publish.initialModel
    , editorModel = Editor.initialModel appState Uuid.nil Nothing
    , migrationModel = Migration.initialModel Uuid.nil
    }


{-| The editor holds a websocket open while it is on screen, so leaving it
closes the socket and forgets the knowledge model; changing tab inside the same
editor keeps both.
-}
leaveEditor : AppState -> Model -> Route -> ( Model, Cmd Msg )
leaveEditor appState model route =
    let
        stayingInSameEditor =
            case route of
                EditorRoute uuid _ ->
                    uuid == model.editorModel.uuid

                _ ->
                    False
    in
    if stayingInSameEditor || model.editorModel.uuid == Uuid.nil then
        ( model, Cmd.none )

    else
        ( { model | editorModel = Editor.initialModel appState Uuid.nil Nothing }
        , Cmd.map EditorMsg (Editor.close model.editorModel)
        )


init : AppState -> Model -> Route -> ( Model, Cmd Msg )
init appState previousModel route =
    let
        ( model, leaveCmd ) =
            leaveEditor appState previousModel route

        withLeaveCmd ( nextModel, cmd ) =
            ( nextModel, Cmd.batch [ leaveCmd, cmd ] )
    in
    withLeaveCmd <|
        case route of
            CreateRoute selectedPackage edit ->
                let
                    ( createModel, cmd ) =
                        Create.init appState selectedPackage edit
                in
                ( { model | createModel = createModel }, Cmd.map CreateMsg cmd )

            IndexRoute paginationQueryString ->
                let
                    ( indexModel, cmd ) =
                        Index.init paginationQueryString
                in
                ( { model | indexModel = indexModel }, Cmd.map IndexMsg cmd )

            PublishRoute uuid ->
                let
                    ( publishModel, cmd ) =
                        Publish.init appState uuid
                in
                ( { model | publishModel = publishModel }, Cmd.map PublishMsg cmd )

            EditorRoute uuid editorRoute ->
                let
                    ( editorModel, cmd ) =
                        Editor.init appState uuid editorRoute model.editorModel
                in
                ( { model | editorModel = editorModel }, Cmd.map EditorMsg cmd )

            MigrationRoute uuid ->
                let
                    ( migrationModel, cmd ) =
                        Migration.init appState uuid
                in
                ( { model | migrationModel = migrationModel }, Cmd.map MigrationMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        CreateMsg subMsg ->
            let
                ( createModel, cmd ) =
                    Create.update (UpdateConfig.map CreateMsg cfg) appState subMsg model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        IndexMsg subMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState subMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        PublishMsg subMsg ->
            let
                ( publishModel, cmd ) =
                    Publish.update (UpdateConfig.map PublishMsg cfg) appState subMsg model.publishModel
            in
            ( appState.seed, { model | publishModel = publishModel }, cmd )

        EditorMsg subMsg ->
            let
                ( seed, editorModel, cmd ) =
                    Editor.update (UpdateConfig.map EditorMsg cfg) appState subMsg model.editorModel
            in
            ( seed, { model | editorModel = editorModel }, cmd )

        MigrationMsg subMsg ->
            let
                ( migrationModel, cmd ) =
                    Migration.update (UpdateConfig.map MigrationMsg cfg) appState subMsg model.migrationModel
            in
            ( appState.seed, { model | migrationModel = migrationModel }, cmd )


subscriptions : AppState -> Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        CreateRoute _ _ ->
            Sub.map CreateMsg (Create.subscriptions appState model.createModel)

        IndexRoute _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)

        PublishRoute _ ->
            Sub.map PublishMsg (Publish.subscriptions appState model.publishModel)

        EditorRoute _ _ ->
            Sub.map EditorMsg (Editor.subscriptions appState model.editorModel)

        MigrationRoute _ ->
            Sub.none


view : AppState -> Route -> Model -> Html Msg
view appState route model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg (Create.view appState model.createModel)

        IndexRoute _ ->
            Html.map IndexMsg (Index.view appState model.indexModel)

        PublishRoute _ ->
            Html.map PublishMsg (Publish.view appState model.publishModel)

        EditorRoute _ editorRoute ->
            Html.map EditorMsg (Editor.view appState editorRoute model.editorModel)

        MigrationRoute _ ->
            Html.map MigrationMsg (Migration.view appState model.migrationModel)


{-| The editor is the only thing with something to lose: an event still on its
way, or a keystroke still waiting for the debounce.
-}
isGuarded : AppState -> Route -> Maybe Routes.Route -> Model -> Maybe String
isGuarded appState route mbNextRoute model =
    case ( route, mbNextRoute ) of
        ( EditorRoute _ _, Just nextRoute ) ->
            Editor.isGuarded appState nextRoute model.editorModel

        _ ->
            Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> Route -> Maybe String
entityName model route =
    case route of
        EditorRoute _ _ ->
            Editor.entityName model.editorModel

        _ ->
            Nothing
