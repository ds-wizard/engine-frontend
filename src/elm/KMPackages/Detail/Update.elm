module KMPackages.Detail.Update exposing (getPackagesFilteredCmd, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMPackages.Detail.Models exposing (..)
import KMPackages.Detail.Msgs exposing (Msg(..))
import KMPackages.Models exposing (PackageDetail)
import KMPackages.Requests exposing (..)
import Msgs
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackageCompleted result ->
            getPackageCompleted model result

        ShowHideDeleteVersion version ->
            ( { model | versionToBeDeleted = version, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion session model

        DeleteVersionCompleted result ->
            deleteVersionCompleted model result


getPackagesFilteredCmd : String -> String -> Session -> Cmd Msgs.Msg
getPackagesFilteredCmd organizationId kmId session =
    getPackagesFiltered organizationId kmId session
        |> toCmd GetPackageCompleted Msgs.PackageManagementDetailMsg


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
                    { model | packages = getServerErrorJwt error "Unable to get package detail" }
    in
    ( newModel, Cmd.none )


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
                            KMPackagesDetail package.organizationId package.kmId

                        _ ->
                            KMPackages
            in
            ( model, cmdNavigate route )

        Err error ->
            ( { model
                | deletingVersion = getServerErrorJwt error "Version could not be deleted"
              }
            , Cmd.none
            )
