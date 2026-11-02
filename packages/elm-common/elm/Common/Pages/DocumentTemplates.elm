module Common.Pages.DocumentTemplates exposing
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

{-| Document Templates: the feature router.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.DocumentTemplates.Detail as Detail
import Common.Pages.DocumentTemplates.Import as Import
import Common.Pages.DocumentTemplates.Index as Index
import Common.Pages.DocumentTemplates.Routes as DocumentTemplatesRoutes exposing (Route(..))
import Html exposing (Html)
import Random


type alias Model =
    { detailModel : Detail.Model
    , importModel : Import.Model
    , indexModel : Index.Model
    }


type Msg
    = DetailMsg Detail.Msg
    | ImportMsg Import.Msg
    | IndexMsg Index.Msg


initialModel : AppState -> Model
initialModel appState =
    { detailModel = Detail.initialModel appState
    , importModel = Import.initialModel appState Nothing
    , indexModel = Index.initialModel PaginationQueryString.empty
    }


init : AppState -> Model -> DocumentTemplatesRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        DetailRoute templateUuid _ ->
            let
                ( detailModel, detailCmd ) =
                    Detail.init appState templateUuid
            in
            ( { model | detailModel = detailModel }, Cmd.map DetailMsg detailCmd )

        ImportRoute documentTemplateId ->
            ( { model | importModel = Import.initialModel appState documentTemplateId }, Cmd.none )

        IndexRoute paginationQueryString ->
            let
                ( indexModel, indexCmd ) =
                    Index.init paginationQueryString
            in
            ( { model | indexModel = indexModel }, Cmd.map IndexMsg indexCmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        DetailMsg detailMsg ->
            let
                ( detailModel, cmd ) =
                    Detail.update (UpdateConfig.map DetailMsg cfg) appState detailMsg model.detailModel
            in
            ( appState.seed, { model | detailModel = detailModel }, cmd )

        ImportMsg importMsg ->
            let
                ( importModel, cmd ) =
                    Import.update (UpdateConfig.map ImportMsg cfg) appState importMsg model.importModel
            in
            ( appState.seed, { model | importModel = importModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )


subscriptions : AppState -> DocumentTemplatesRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        DetailRoute _ _ ->
            Sub.map DetailMsg (Detail.subscriptions appState model.detailModel)

        ImportRoute _ ->
            Sub.map ImportMsg (Import.subscriptions appState model.importModel)

        IndexRoute _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)


view : RowPolicy -> AppState -> DocumentTemplatesRoutes.Route -> Model -> Html Msg
view policy appState route model =
    case route of
        DetailRoute _ detailRoute ->
            Html.map DetailMsg (Detail.view policy appState detailRoute model.detailModel)

        ImportRoute _ ->
            Html.map ImportMsg (Import.view appState model.importModel)

        IndexRoute _ ->
            Html.map IndexMsg (Index.view policy appState model.indexModel)


isGuarded : AppState -> DocumentTemplatesRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> DocumentTemplatesRoutes.Route -> Maybe String
entityName model route =
    case route of
        DetailRoute _ _ ->
            Detail.entityName model.detailModel

        _ ->
            Nothing
