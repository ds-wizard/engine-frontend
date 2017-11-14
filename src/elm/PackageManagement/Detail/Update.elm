module PackageManagement.Detail.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Jwt
import Msgs
import PackageManagement.Detail.Models exposing (..)
import PackageManagement.Detail.Msgs exposing (Msg(..))
import PackageManagement.Models exposing (PackageDetail)
import PackageManagement.Requests exposing (..)
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)


getPackagesFilteredCmd : String -> String -> Session -> Cmd Msgs.Msg
getPackagesFilteredCmd groupId artifactId session =
    getPackagesFiltered groupId artifactId session
        |> toCmd GetPackageCompleted Msgs.PackageManagementDetailMsg


deletePackageCmd : String -> String -> Session -> Cmd Msgs.Msg
deletePackageCmd groupId artifactId session =
    deletePackage groupId artifactId session
        |> toCmd DeletePackageCompleted Msgs.PackageManagementDetailMsg


deletePackageVersionCmd : String -> Session -> Cmd Msgs.Msg
deletePackageVersionCmd packageId session =
    deletePackageVersion packageId session
        |> toCmd DeleteVersionCompleted Msgs.PackageManagementDetailMsg


getPackageCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = Success packages }

                Err error ->
                    { model | packages = Error "Unable to get package detail" }
    in
    ( newModel, Cmd.none )


handleDeletePackage : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeletePackage session model =
    case currentPackage model of
        Just package ->
            ( { model | deletingPackage = Loading }
            , deletePackageCmd package.groupId package.artifactId session
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deletePackageCompleted model result =
    case result of
        Ok package ->
            ( model, cmdNavigate PackageManagement )

        Err error ->
            ( { model | deletingPackage = Error "Package could not be deleted" }
            , Cmd.none
            )


handleDeleteVersion : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteVersion session model =
    case ( currentPackage model, model.versionToBeDeleted ) of
        ( Just package, Just version ) ->
            ( { model | deletingVersion = Loading }
            , deletePackageVersionCmd version session
            )

        _ ->
            ( model, Cmd.none )


deleteVersionCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteVersionCompleted model result =
    case result of
        Ok version ->
            let
                route =
                    case ( packagesLength model > 1, currentPackage model ) of
                        ( True, Just package ) ->
                            PackageManagementDetail package.groupId package.artifactId

                        _ ->
                            PackageManagement
            in
            ( model, cmdNavigate route )

        Err error ->
            ( { model
                | deletingVersion = Error "Version could not be deleted"
              }
            , Cmd.none
            )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackageCompleted result ->
            getPackageCompleted model result

        ShowHideDeleteDialog value ->
            ( { model | showDeleteDialog = value, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage session model

        DeletePackageCompleted result ->
            deletePackageCompleted model result

        ShowHideDeleteVersion version ->
            ( { model | versionToBeDeleted = version, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion session model

        DeleteVersionCompleted result ->
            deleteVersionCompleted model result
