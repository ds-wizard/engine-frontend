module KMPackages.Index.Update exposing (getPackagesCmd, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMPackages.Index.Models exposing (Model)
import KMPackages.Index.Msgs exposing (Msg(..))
import KMPackages.Models exposing (Package)
import KMPackages.Requests exposing (deletePackage, getPackagesUnique)
import Msgs
import Requests exposing (toCmd)


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result

        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage session model

        DeletePackageCompleted result ->
            deletePackageCompleted session model result


getPackagesCmd : Session -> Cmd Msgs.Msg
getPackagesCmd session =
    getPackagesUnique session
        |> toCmd GetPackagesCompleted Msgs.PackageManagementIndexMsg


getPackagesCompleted : Model -> Result Jwt.JwtError (List Package) -> ( Model, Cmd Msgs.Msg )
getPackagesCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = Success packages }

                Err error ->
                    { model | packages = getServerErrorJwt error "Unable to fetch package list" }
    in
    ( newModel, Cmd.none )


handleDeletePackage : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeletePackage session model =
    case model.packageToBeDeleted of
        Just package ->
            ( { model | deletingPackage = Loading }
            , deletePackageCmd package.organizationId package.kmId session
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCmd : String -> String -> Session -> Cmd Msgs.Msg
deletePackageCmd organizationId kmId session =
    deletePackage organizationId kmId session
        |> toCmd DeletePackageCompleted Msgs.PackageManagementIndexMsg


deletePackageCompleted : Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deletePackageCompleted session model result =
    case result of
        Ok package ->
            ( { model
                | deletingPackage = Success "Package and all its versions were sucessfully deleted"
                , packages = Loading
                , packageToBeDeleted = Nothing
              }
            , getPackagesCmd session
            )

        Err error ->
            ( { model | deletingPackage = getServerErrorJwt error "Package could not be deleted" }
            , Cmd.none
            )
