module Common.Pages.Tenants exposing
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

{-| Tenants: the feature router.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Tenants.Create as Create
import Common.Pages.Tenants.Detail as Detail
import Common.Pages.Tenants.Index as Index
import Common.Pages.Tenants.Routes as TenantsRoutes
import Html exposing (Html)
import Random
import Uuid


type alias Model =
    { indexModel : Index.Model
    , createModel : Create.Model
    , detailModel : Detail.Model
    }


type Msg
    = IndexMsg Index.Msg
    | CreateMsg Create.Msg
    | DetailMsg Detail.Msg


initialModel : AppState -> Model
initialModel appState =
    { indexModel = Index.initialModel PaginationQueryString.empty Nothing Nothing
    , createModel = Create.initialModel
    , detailModel = Detail.initialModel appState Uuid.nil
    }


init : AppState -> Model -> TenantsRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        TenantsRoutes.IndexRoute paginationQueryString mbEnabled mbStates ->
            let
                ( indexModel, cmd ) =
                    Index.init paginationQueryString mbEnabled mbStates
            in
            ( { model | indexModel = indexModel }, Cmd.map IndexMsg cmd )

        TenantsRoutes.CreateRoute ->
            let
                ( createModel, cmd ) =
                    Create.init
            in
            ( { model | createModel = createModel }, Cmd.map CreateMsg cmd )

        TenantsRoutes.DetailRoute uuid ->
            let
                ( detailModel, cmd ) =
                    Detail.init appState uuid
            in
            ( { model | detailModel = detailModel }, Cmd.map DetailMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        CreateMsg createMsg ->
            let
                ( createModel, cmd ) =
                    Create.update (UpdateConfig.map CreateMsg cfg) appState createMsg model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        DetailMsg detailMsg ->
            let
                ( detailModel, cmd ) =
                    Detail.update (UpdateConfig.map DetailMsg cfg) appState detailMsg model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )


subscriptions : AppState -> TenantsRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        TenantsRoutes.IndexRoute _ _ _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)

        TenantsRoutes.CreateRoute ->
            Sub.map CreateMsg (Create.subscriptions appState model.createModel)

        TenantsRoutes.DetailRoute _ ->
            Sub.map DetailMsg (Detail.subscriptions appState model.detailModel)


view : AppState -> TenantsRoutes.Route -> Model -> Html Msg
view appState route model =
    case route of
        TenantsRoutes.IndexRoute _ _ _ ->
            Html.map IndexMsg (Index.view appState model.indexModel)

        TenantsRoutes.CreateRoute ->
            Html.map CreateMsg (Create.view appState model.createModel)

        TenantsRoutes.DetailRoute _ ->
            Html.map DetailMsg (Detail.view appState model.detailModel)


isGuarded : AppState -> TenantsRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> TenantsRoutes.Route -> Maybe String
entityName model route =
    case route of
        TenantsRoutes.DetailRoute _ ->
            Detail.entityName model.detailModel

        _ ->
            Nothing
