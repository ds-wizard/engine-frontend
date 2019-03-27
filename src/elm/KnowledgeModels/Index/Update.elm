module KnowledgeModels.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import KnowledgeModels.Common.Models exposing (Package)
import KnowledgeModels.Index.Models exposing (Model)
import KnowledgeModels.Index.Msgs exposing (Msg(..))
import Msgs


fetchData : (Msg -> Msgs.Msg) -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg appState =
    Cmd.map wrapMsg <|
        PackagesApi.getPackagesUnique appState GetPackagesCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result

        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage wrapMsg appState model

        DeletePackageCompleted result ->
            deletePackageCompleted wrapMsg appState model result


getPackagesCompleted : Model -> Result ApiError (List Package) -> ( Model, Cmd Msgs.Msg )
getPackagesCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = Success packages }

                Err error ->
                    { model | packages = getServerError error "Unable to fetch package list" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


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
        Ok package ->
            ( { model
                | deletingPackage = Success "Package and all its versions were sucessfully deleted"
                , packages = Loading
                , packageToBeDeleted = Nothing
              }
            , fetchData wrapMsg appState
            )

        Err error ->
            ( { model | deletingPackage = getServerError error "Package could not be deleted" }
            , getResultCmd result
            )
