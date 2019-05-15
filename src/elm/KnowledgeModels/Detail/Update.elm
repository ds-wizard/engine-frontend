module KnowledgeModels.Detail.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Common.Api exposing (getResultCmd)
import Common.Api.Packages as PackagesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import KnowledgeModels.Common.Models exposing (PackageDetail)
import KnowledgeModels.Detail.Models exposing (..)
import KnowledgeModels.Detail.Msgs exposing (Msg(..))
import KnowledgeModels.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..), cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> String -> String -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg organizationId kmId appState =
    Cmd.map wrapMsg <|
        PackagesApi.getPackagesFiltered organizationId kmId appState GetPackageCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetPackageCompleted result ->
            getPackageCompleted model result

        ShowHideDeleteVersion version ->
            ( { model | versionToBeDeleted = version, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            deleteVersionCompleted appState model result

        DropdownMsg packageDetail dropdownState ->
            handleDropdownToggle model packageDetail dropdownState


getPackageCompleted : Model -> Result ApiError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = Success <| List.map initPackageDetailRow packages }

                Err error ->
                    { model | packages = getServerError error "Unable to get knowledge model detail" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleDeleteVersion : (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case ( currentPackage model, model.versionToBeDeleted ) of
        ( Just package, Just version ) ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| PackagesApi.deletePackageVersion version appState DeleteVersionCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
deleteVersionCompleted appState model result =
    case result of
        Ok version ->
            let
                route =
                    case ( packagesLength model > 1, currentPackage model ) of
                        ( True, Just package ) ->
                            KnowledgeModels <| Detail package.organizationId package.kmId

                        _ ->
                            KnowledgeModels Index
            in
            ( model, cmdNavigate appState.key route )

        Err error ->
            ( { model
                | deletingVersion = getServerError error "Version could not be deleted"
              }
            , getResultCmd result
            )


handleDropdownToggle : Model -> PackageDetail -> Dropdown.State -> ( Model, Cmd Msgs.Msg )
handleDropdownToggle model packageDetail appState =
    case model.packages of
        Success packageDetailRows ->
            let
                replaceWith row =
                    if row.packageDetail == packageDetail then
                        { row | dropdownState = appState }

                    else
                        row

                newRows =
                    List.map replaceWith packageDetailRows
            in
            ( { model | packages = Success newRows }, Cmd.none )

        _ ->
            ( model, Cmd.none )
