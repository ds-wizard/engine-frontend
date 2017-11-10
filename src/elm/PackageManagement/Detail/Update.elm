module PackageManagement.Detail.Update exposing (..)

import Auth.Models exposing (Session)
import Jwt
import Msgs
import PackageManagement.Detail.Models exposing (Model)
import PackageManagement.Detail.Msgs exposing (Msg(..))
import PackageManagement.Models exposing (PackageDetail, getPackageName)
import PackageManagement.Requests exposing (deletePackage, getPackage)
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import Tuple


getPackageCmd : String -> Session -> Cmd Msgs.Msg
getPackageCmd shortName session =
    getPackage shortName session
        |> toCmd GetPackageCompleted Msgs.PackageManagementDetailMsg


deletePackageCmd : String -> Session -> Cmd Msgs.Msg
deletePackageCmd shortName session =
    deletePackage shortName session
        |> toCmd DeletePackageCompleted Msgs.PackageManagementDetailMsg


getPackageCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = packages }

                Err error ->
                    { model | error = "Unable to get package detail" }
    in
    ( { newModel | loading = False }, Cmd.none )


handleDeletePackage : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeletePackage session model =
    let
        shortName =
            model.packages |> getPackageName |> Tuple.second
    in
    ( { model | deletingPackage = True, deleteError = "" }, deletePackageCmd shortName session )


deletePackageCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deletePackageCompleted model result =
    case result of
        Ok package ->
            ( model, cmdNavigate PackageManagement )

        Err error ->
            ( { model
                | deletingPackage = False
                , deleteError = "Package could not be deleted"
              }
            , Cmd.none
            )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackageCompleted result ->
            getPackageCompleted model result

        ShowHideDeleteDialog value ->
            ( { model | showDeleteDialog = value }, Cmd.none )

        DeletePackage ->
            handleDeletePackage session model

        DeletePackageCompleted result ->
            deletePackageCompleted model result
