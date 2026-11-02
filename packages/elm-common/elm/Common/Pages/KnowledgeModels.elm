module Common.Pages.KnowledgeModels exposing
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

{-| Knowledge Models: the feature router.

The index, the detail with its readme and locales tabs, the import, the
resource page, the preview and the comparison.

-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.RowPolicy exposing (RowPolicy)
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.KnowledgeModels.Compare as Compare
import Common.Pages.KnowledgeModels.Detail as Detail
import Common.Pages.KnowledgeModels.Import as Import
import Common.Pages.KnowledgeModels.Index as Index
import Common.Pages.KnowledgeModels.Preview as Preview
import Common.Pages.KnowledgeModels.ResourcePage as ResourcePage
import Common.Pages.KnowledgeModels.Routes as KnowledgeModelsRoutes
import Html exposing (Html)
import Random


type alias Model =
    { indexModel : Index.Model
    , detailModel : Detail.Model
    , importModel : Import.Model
    , resourcePageModel : ResourcePage.Model
    , previewModel : Preview.Model
    , compareModel : Compare.Model
    }


type Msg
    = IndexMsg Index.Msg
    | DetailMsg Detail.Msg
    | ImportMsg Import.Msg
    | ResourcePageMsg ResourcePage.Msg
    | PreviewMsg Preview.Msg
    | CompareMsg Compare.Msg


initialModel : AppState -> Model
initialModel appState =
    { indexModel = Index.initialModel PaginationQueryString.empty
    , detailModel = Detail.initialModel appState
    , importModel = Import.initialModel appState Nothing
    , resourcePageModel = ResourcePage.initialModel ""
    , previewModel = Preview.initialModel Nothing
    , compareModel = Compare.initialModel appState Nothing
    }


init : AppState -> Model -> KnowledgeModelsRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        KnowledgeModelsRoutes.IndexRoute paginationQueryString ->
            let
                ( indexModel, cmd ) =
                    Index.init paginationQueryString
            in
            ( { model | indexModel = indexModel }, Cmd.map IndexMsg cmd )

        KnowledgeModelsRoutes.DetailRoute kmPackageUuid _ ->
            let
                ( detailModel, cmd ) =
                    Detail.init appState kmPackageUuid
            in
            ( { model | detailModel = detailModel }, Cmd.map DetailMsg cmd )

        KnowledgeModelsRoutes.ImportRoute kmPackageId ->
            let
                ( importModel, cmd ) =
                    Import.init appState kmPackageId
            in
            ( { model | importModel = importModel }, Cmd.map ImportMsg cmd )

        KnowledgeModelsRoutes.ResourcePageRoute kmUuid resourcePageUuid ->
            let
                ( resourcePageModel, cmd ) =
                    ResourcePage.init appState kmUuid resourcePageUuid
            in
            ( { model | resourcePageModel = resourcePageModel }, Cmd.map ResourcePageMsg cmd )

        KnowledgeModelsRoutes.PreviewRoute kmPackageUuid mbQuestionUuid ->
            let
                ( previewModel, cmd ) =
                    Preview.init appState kmPackageUuid mbQuestionUuid
            in
            ( { model | previewModel = previewModel }, Cmd.map PreviewMsg cmd )

        KnowledgeModelsRoutes.CompareRoute mbLeftKmPackageUuid ->
            let
                ( compareModel, cmd ) =
                    Compare.init appState mbLeftKmPackageUuid
            in
            ( { model | compareModel = compareModel }, Cmd.map CompareMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Index.update (UpdateConfig.map IndexMsg cfg) appState indexMsg model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )

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

        ResourcePageMsg resourcePageMsg ->
            let
                ( resourcePageModel, cmd ) =
                    ResourcePage.update (UpdateConfig.map ResourcePageMsg cfg) appState resourcePageMsg model.resourcePageModel
            in
            ( appState.seed, { model | resourcePageModel = resourcePageModel }, cmd )

        PreviewMsg previewMsg ->
            let
                ( seed, previewModel, cmd ) =
                    Preview.update (UpdateConfig.map PreviewMsg cfg) appState previewMsg model.previewModel
            in
            ( seed, { model | previewModel = previewModel }, cmd )

        CompareMsg compareMsg ->
            let
                ( compareModel, cmd ) =
                    Compare.update (UpdateConfig.map CompareMsg cfg) appState compareMsg model.compareModel
            in
            ( appState.seed, { model | compareModel = compareModel }, cmd )


subscriptions : AppState -> KnowledgeModelsRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        KnowledgeModelsRoutes.IndexRoute _ ->
            Sub.map IndexMsg (Index.subscriptions model.indexModel)

        KnowledgeModelsRoutes.DetailRoute _ _ ->
            Sub.map DetailMsg (Detail.subscriptions appState model.detailModel)

        KnowledgeModelsRoutes.ImportRoute _ ->
            Sub.map ImportMsg (Import.subscriptions appState model.importModel)

        KnowledgeModelsRoutes.PreviewRoute _ _ ->
            Sub.map PreviewMsg (Preview.subscriptions model.previewModel)

        KnowledgeModelsRoutes.CompareRoute _ ->
            Sub.map CompareMsg (Compare.subscriptions model.compareModel)

        _ ->
            Sub.none


view : RowPolicy -> AppState -> KnowledgeModelsRoutes.Route -> Model -> Html Msg
view policy appState route model =
    case route of
        KnowledgeModelsRoutes.IndexRoute _ ->
            Html.map IndexMsg (Index.view policy appState model.indexModel)

        KnowledgeModelsRoutes.DetailRoute _ detailRoute ->
            Html.map DetailMsg (Detail.view policy appState detailRoute model.detailModel)

        KnowledgeModelsRoutes.ImportRoute _ ->
            Html.map ImportMsg (Import.view appState model.importModel)

        KnowledgeModelsRoutes.ResourcePageRoute _ _ ->
            Html.map ResourcePageMsg (ResourcePage.view appState model.resourcePageModel)

        KnowledgeModelsRoutes.PreviewRoute _ _ ->
            Html.map PreviewMsg (Preview.view policy appState model.previewModel)

        KnowledgeModelsRoutes.CompareRoute _ ->
            Html.map CompareMsg (Compare.view appState model.compareModel)


isGuarded : AppState -> KnowledgeModelsRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Model -> KnowledgeModelsRoutes.Route -> Maybe String
entityName model route =
    case route of
        KnowledgeModelsRoutes.DetailRoute _ _ ->
            Detail.entityName model.detailModel

        _ ->
            Nothing
