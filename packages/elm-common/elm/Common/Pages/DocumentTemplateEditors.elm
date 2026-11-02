module Common.Pages.DocumentTemplateEditors exposing
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

{-| Document Template Editors: the feature router.

The index, create and the editor's three tabs.

-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.DocumentTemplateEditors.Create as Create
import Common.Pages.DocumentTemplateEditors.Editor as Editor
import Common.Pages.DocumentTemplateEditors.Index as Index
import Common.Pages.DocumentTemplateEditors.Routes as DocumentTemplateEditorsRoutes exposing (Route(..))
import Common.Routes as Routes
import Html exposing (Html)
import Random
import Uuid


type alias Model =
    { createModel : Create.Model
    , indexModel : Index.Model
    , editorModel : Editor.Model
    }


type Msg
    = CreateMsg Create.Msg
    | IndexMsg Index.Msg
    | EditorMsg Editor.Msg


initialModel : AppState -> Model
initialModel appState =
    { createModel = Create.initialModel appState Nothing Nothing
    , indexModel = Index.initialModel PaginationQueryString.empty
    , editorModel = Editor.initialModel appState Uuid.nil
    }


init : AppState -> Model -> DocumentTemplateEditorsRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        CreateRoute selectedDocumentTemplate edit ->
            let
                ( createModel, createCmd ) =
                    Create.init appState selectedDocumentTemplate edit
            in
            ( { model | createModel = createModel }, Cmd.map CreateMsg createCmd )

        IndexRoute paginationQueryString ->
            let
                ( indexModel, indexCmd ) =
                    Index.init paginationQueryString
            in
            ( { model | indexModel = indexModel }, Cmd.map IndexMsg indexCmd )

        EditorRoute uuid editorRoute ->
            let
                ( editorModel, cmd ) =
                    Editor.init appState uuid editorRoute model.editorModel
            in
            ( { model | editorModel = editorModel }, Cmd.map EditorMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        CreateMsg createMsg ->
            let
                ( createModel, cmd ) =
                    Create.update (UpdateConfig.map CreateMsg cfg) appState createMsg model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        EditorMsg subMsg ->
            let
                ( seed, editorModel, cmd ) =
                    Editor.update (UpdateConfig.map EditorMsg cfg) appState subMsg model.editorModel
            in
            ( seed, { model | editorModel = editorModel }, cmd )


subscriptions : AppState -> DocumentTemplateEditorsRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        CreateRoute _ _ ->
            Sub.map CreateMsg (Create.subscriptions appState model.createModel)

        IndexRoute _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)

        EditorRoute _ _ ->
            Sub.map EditorMsg (Editor.subscriptions appState model.editorModel)


view : AppState -> DocumentTemplateEditorsRoutes.Route -> Model -> Html Msg
view appState route model =
    case route of
        CreateRoute _ _ ->
            Html.map CreateMsg (Create.view appState model.createModel)

        IndexRoute _ ->
            Html.map IndexMsg (Index.view appState model.indexModel)

        EditorRoute _ editorRoute ->
            Html.map EditorMsg (Editor.view appState editorRoute model.editorModel)


{-| The editor is the only thing with something to lose: a file typed into and
not yet saved.
-}
isGuarded : AppState -> DocumentTemplateEditorsRoutes.Route -> Maybe Routes.Route -> Model -> Maybe String
isGuarded appState route mbNextRoute model =
    case ( route, mbNextRoute ) of
        ( EditorRoute _ _, Just nextRoute ) ->
            Editor.isGuarded appState nextRoute model.editorModel

        _ ->
            Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> DocumentTemplateEditorsRoutes.Route -> Maybe String
entityName model route =
    case route of
        EditorRoute _ _ ->
            Editor.entityName model.editorModel

        _ ->
            Nothing
