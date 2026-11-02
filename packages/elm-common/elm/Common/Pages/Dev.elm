module Common.Pages.Dev exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| Dev: the feature router.
-}

import Common.Data.AppState exposing (AppState)
import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Dev.Operations as Operations
import Common.Pages.Dev.PersistentCommandsDetail as PersistentCommandsDetail
import Common.Pages.Dev.PersistentCommandsIndex as PersistentCommandsIndex
import Common.Pages.Dev.Routes as DevRoutes
import Html exposing (Html)
import Random
import Uuid


type alias Model =
    { operationsModel : Operations.Model
    , persistentCommandsDetailModel : PersistentCommandsDetail.Model
    , persistentCommandsIndexModel : PersistentCommandsIndex.Model
    }


type Msg
    = OperationsMsg Operations.Msg
    | PersistentCommandsDetailMsg PersistentCommandsDetail.Msg
    | PersistentCommandsIndexMsg PersistentCommandsIndex.Msg


initialModel : AppState -> Model
initialModel appState =
    { operationsModel = Operations.initialModel
    , persistentCommandsDetailModel = PersistentCommandsDetail.initialModel appState Uuid.nil
    , persistentCommandsIndexModel = PersistentCommandsIndex.initialModel PaginationQueryString.empty Nothing
    }


init : AppState -> Model -> DevRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        DevRoutes.OperationsRoute mbSection ->
            let
                ( operationsModel, cmd ) =
                    Operations.init appState mbSection model.operationsModel
            in
            ( { model | operationsModel = operationsModel }, Cmd.map OperationsMsg cmd )

        DevRoutes.PersistentCommandsDetail uuid ->
            let
                ( detailModel, cmd ) =
                    PersistentCommandsDetail.init appState uuid
            in
            ( { model | persistentCommandsDetailModel = detailModel }, Cmd.map PersistentCommandsDetailMsg cmd )

        DevRoutes.PersistentCommandsIndex paginationQueryString mbState ->
            let
                ( indexModel, cmd ) =
                    PersistentCommandsIndex.init paginationQueryString mbState
            in
            ( { model | persistentCommandsIndexModel = indexModel }, Cmd.map PersistentCommandsIndexMsg cmd )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        OperationsMsg operationsMsg ->
            let
                ( operationsModel, cmd ) =
                    Operations.update (UpdateConfig.map OperationsMsg cfg) appState operationsMsg model.operationsModel
            in
            ( appState.seed, { model | operationsModel = operationsModel }, cmd )

        PersistentCommandsDetailMsg detailMsg ->
            let
                ( detailModel, cmd ) =
                    PersistentCommandsDetail.update (UpdateConfig.map PersistentCommandsDetailMsg cfg) appState detailMsg model.persistentCommandsDetailModel
            in
            ( appState.seed, { model | persistentCommandsDetailModel = detailModel }, cmd )

        PersistentCommandsIndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    PersistentCommandsIndex.update (UpdateConfig.map PersistentCommandsIndexMsg cfg) appState indexMsg model.persistentCommandsIndexModel
            in
            ( appState.seed, { model | persistentCommandsIndexModel = indexModel }, cmd )


subscriptions : AppState -> DevRoutes.Route -> Model -> Sub Msg
subscriptions =
    \_ route model ->
        case route of
            DevRoutes.OperationsRoute _ ->
                Sub.none

            DevRoutes.PersistentCommandsDetail _ ->
                Sub.map PersistentCommandsDetailMsg (PersistentCommandsDetail.subscriptions model.persistentCommandsDetailModel)

            DevRoutes.PersistentCommandsIndex _ _ ->
                Sub.map PersistentCommandsIndexMsg (PersistentCommandsIndex.subscriptions model.persistentCommandsIndexModel)


view : AppState -> DevRoutes.Route -> Model -> Html Msg
view appState route model =
    case route of
        DevRoutes.OperationsRoute _ ->
            Html.map OperationsMsg (Operations.view appState model.operationsModel)

        DevRoutes.PersistentCommandsDetail _ ->
            Html.map PersistentCommandsDetailMsg (PersistentCommandsDetail.view appState model.persistentCommandsDetailModel)

        DevRoutes.PersistentCommandsIndex _ _ ->
            Html.map PersistentCommandsIndexMsg (PersistentCommandsIndex.view appState model.persistentCommandsIndexModel)


isGuarded : AppState -> DevRoutes.Route -> Model -> Maybe String
isGuarded =
    \_ _ _ ->
        Nothing
