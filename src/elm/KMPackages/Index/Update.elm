module KMPackages.Index.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMPackages.Common.Models exposing (Package)
import KMPackages.Index.Models exposing (Model)
import KMPackages.Index.Msgs exposing (Msg(..))
import KMPackages.Requests exposing (deletePackage, getPackagesUnique)
import Msgs


fetchData : (Msg -> Msgs.Msg) -> Session -> Cmd Msgs.Msg
fetchData wrapMsg session =
    getPackagesUnique session
        |> Jwt.send GetPackagesCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result

        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage wrapMsg session model

        DeletePackageCompleted result ->
            deletePackageCompleted wrapMsg session model result


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


handleDeletePackage : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeletePackage wrapMsg session model =
    case model.packageToBeDeleted of
        Just package ->
            ( { model | deletingPackage = Loading }
            , deletePackageCmd wrapMsg package.organizationId package.kmId session
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCmd : (Msg -> Msgs.Msg) -> String -> String -> Session -> Cmd Msgs.Msg
deletePackageCmd wrapMsg organizationId kmId session =
    deletePackage organizationId kmId session
        |> Jwt.send DeletePackageCompleted
        |> Cmd.map wrapMsg


deletePackageCompleted : (Msg -> Msgs.Msg) -> Session -> Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deletePackageCompleted wrapMsg session model result =
    case result of
        Ok package ->
            ( { model
                | deletingPackage = Success "Package and all its versions were sucessfully deleted"
                , packages = Loading
                , packageToBeDeleted = Nothing
              }
            , fetchData wrapMsg session
            )

        Err error ->
            ( { model | deletingPackage = getServerErrorJwt error "Package could not be deleted" }
            , Cmd.none
            )
