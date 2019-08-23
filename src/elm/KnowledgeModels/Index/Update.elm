module KnowledgeModels.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (applyResult, getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lg)
import Common.Setters exposing (setPackages)
import KnowledgeModels.Index.Models exposing (Model)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import Msgs


l_ : String -> AppState -> String
l_ =
    l "KnowledgeModels.Index.Update"


fetchData : AppState -> Cmd Msg
fetchData appState =
    PackagesApi.getPackages appState GetPackagesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackagesCompleted result ->
            applyResult
                { setResult = setPackages
                , defaultError = lg "apiError.packages.getListError" appState
                , model = model
                , result = result
                }

        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage wrapMsg appState model

        DeletePackageCompleted result ->
            deletePackageCompleted wrapMsg appState model result


handleDeletePackage : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeletePackage wrapMsg appState model =
    case model.packageToBeDeleted of
        Just package ->
            ( { model | deletingPackage = Loading }
            , Cmd.map wrapMsg <|
                PackagesApi.deletePackage package.organizationId package.kmId appState DeletePackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCompleted : (Msg -> Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deletePackageCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingPackage = Success <| lg "apiSuccess.packages.delete" appState
                , packages = Loading
                , packageToBeDeleted = Nothing
              }
            , Cmd.map wrapMsg <| fetchData appState
            )

        Err error ->
            ( { model | deletingPackage = getServerError error <| lg "apiError.packages.deleteError" appState }
            , getResultCmd result
            )
