module Common.Pages.UserGroups exposing
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

{-| User groups: the feature router. A group belongs to the workspace it was
made in; which one that is, the application's scope says.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.UserGroups.Create as Create
import Common.Pages.UserGroups.Detail as Detail
import Common.Pages.UserGroups.Index as Index
import Common.Pages.UserGroups.Routes exposing (Route(..))
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


initialModel : Model
initialModel =
    { indexModel = Index.initialModel PaginationQueryString.empty Nothing
    , createModel = Create.initialModel
    , detailModel = Detail.initialModel Uuid.nil
    }


init : AppState -> Model -> Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        Index pqs mbMember ->
            let
                ( indexModel, cmd ) =
                    Index.init pqs mbMember
            in
            ( { model | indexModel = indexModel }, Cmd.map IndexMsg cmd )

        Create ->
            let
                ( createModel, cmd ) =
                    Create.init
            in
            ( { model | createModel = createModel }, Cmd.map CreateMsg cmd )

        Detail uuid _ ->
            let
                ( detailModel, cmd ) =
                    Detail.init appState uuid
            in
            ( { model | detailModel = detailModel }, Cmd.map DetailMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        IndexMsg subMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState subMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

        CreateMsg subMsg ->
            let
                ( createModel, cmd ) =
                    Create.update (UpdateConfig.map CreateMsg cfg) appState subMsg model.createModel
            in
            ( appState.seed, { model | createModel = createModel }, cmd )

        DetailMsg subMsg ->
            let
                ( detailModel, cmd ) =
                    Detail.update (UpdateConfig.map DetailMsg cfg) appState subMsg model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )


subscriptions : AppState -> Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        Index _ _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)

        Create ->
            Sub.map CreateMsg (Create.subscriptions appState model.createModel)

        Detail _ tab ->
            Sub.map DetailMsg (Detail.subscriptions appState tab model.detailModel)


view : AppState -> Route -> Model -> Html Msg
view appState route model =
    case route of
        Index _ _ ->
            Html.map IndexMsg (Index.view appState model.indexModel)

        Create ->
            Html.map CreateMsg (Create.view appState model.createModel)

        Detail _ tab ->
            Html.map DetailMsg (Detail.view appState tab model.detailModel)


{-| A group's details or members with something unsaved ask before leaving —
including for the group's other tab, which loads it afresh.
-}
isGuarded : AppState -> Route -> Model -> Maybe String
isGuarded appState route model =
    case route of
        Detail _ _ ->
            Detail.isGuarded appState model.detailModel

        _ ->
            Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> Route -> Maybe String
entityName model route =
    case route of
        Detail _ _ ->
            Detail.entityName model.detailModel

        _ ->
            Nothing
