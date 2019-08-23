module KnowledgeModels.Detail.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lg)
import Common.Setters exposing (setPackage)
import KnowledgeModels.Detail.Models exposing (..)
import KnowledgeModels.Detail.Msgs exposing (Msg(..))
import KnowledgeModels.Routes exposing (Route(..))
import Msgs
import Routes
import Routing exposing (cmdNavigate)


fetchData : String -> AppState -> Cmd Msg
fetchData packageId appState =
    PackagesApi.getPackage packageId appState GetPackageCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackageCompleted result ->
            applyResult
                { setResult = setPackage
                , defaultError = lg "apiError.packages.getError" appState
                , model = model
                , result = result
                }

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            deleteVersionCompleted appState model result


handleDeleteVersion : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.package of
        Success package ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| PackagesApi.deletePackageVersion package.id appState DeleteVersionCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteVersionCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState <| Routes.KnowledgeModelsRoute IndexRoute )

        Err error ->
            ( { model | deletingVersion = getServerError error <| lg "apiError.packages.deleteError" appState }
            , getResultCmd result
            )
