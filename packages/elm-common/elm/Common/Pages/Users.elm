module Common.Pages.Users exposing
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

{-| Users: the feature router. The listing, creating a user, and
the user settings with their tabs.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.Users.Create as Create
import Common.Pages.Users.Edit as Edit
import Common.Pages.Users.Index as Index
import Common.Pages.Users.Routes as UsersRoutes
import Html exposing (Html)
import Random


type alias Model =
    { createModel : Create.Model
    , editModel : Edit.Model
    , indexModel : Index.Model
    }


type Msg
    = CreateMsg Create.Msg
    | EditMsg Edit.Msg
    | IndexMsg Index.Msg


initialModel : AppState -> Model
initialModel appState =
    { createModel = Create.initialModel appState
    , editModel = Edit.initialModel appState UuidOrCurrent.empty
    , indexModel = Index.initialModel PaginationQueryString.empty Nothing
    }


init : AppState -> Model -> UsersRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        UsersRoutes.CreateRoute ->
            let
                ( createModel, cmd ) =
                    Create.init appState
            in
            ( { model | createModel = createModel }, Cmd.map CreateMsg cmd )

        UsersRoutes.EditRoute uuidOrCurrent subroute ->
            let
                ( editModel, cmd ) =
                    Edit.init appState uuidOrCurrent subroute model.editModel
            in
            ( { model | editModel = editModel }, Cmd.map EditMsg cmd )

        UsersRoutes.IndexRoute paginationQueryString mbRole ->
            let
                ( indexModel, cmd ) =
                    Index.init appState paginationQueryString mbRole
            in
            ( { model | indexModel = indexModel }, Cmd.map IndexMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        CreateMsg createMsg ->
            let
                ( seed, createModel, cmd ) =
                    Create.update (UpdateConfig.map CreateMsg cfg) appState createMsg model.createModel
            in
            ( seed, { model | createModel = createModel }, cmd )

        EditMsg editMsg ->
            let
                ( editModel, cmd ) =
                    Edit.update (UpdateConfig.map EditMsg cfg) appState editMsg model.editModel
            in
            ( appState.seed, { model | editModel = editModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )


subscriptions : AppState -> UsersRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        UsersRoutes.CreateRoute ->
            Sub.map CreateMsg (Create.subscriptions appState model.createModel)

        UsersRoutes.EditRoute _ subroute ->
            Sub.map EditMsg (Edit.subscriptions appState subroute model.editModel)

        UsersRoutes.IndexRoute _ _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)


view : AppState -> UsersRoutes.Route -> Model -> Html Msg
view appState route model =
    case route of
        UsersRoutes.CreateRoute ->
            Html.map CreateMsg (Create.view appState model.createModel)

        UsersRoutes.EditRoute _ subroute ->
            Html.map EditMsg (Edit.view appState subroute model.editModel)

        UsersRoutes.IndexRoute _ _ ->
            Html.map IndexMsg (Index.view appState model.indexModel)


{-| The wizard guards nothing in users: none of its pages asked before leaving.
-}
isGuarded : AppState -> UsersRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> UsersRoutes.Route -> Maybe String
entityName model route =
    case route of
        UsersRoutes.EditRoute _ _ ->
            Edit.entityName model.editModel

        _ ->
            Nothing
