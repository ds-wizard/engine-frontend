module PackageManagement.Detail.Update exposing (..)

import Auth.Models exposing (Session)
import Jwt
import Msgs
import PackageManagement.Detail.Models exposing (Model)
import PackageManagement.Detail.Msgs exposing (Msg(..))
import PackageManagement.Models exposing (PackageDetail)
import PackageManagement.Requests exposing (getPackage)
import Requests exposing (toCmd)


getPackageCmd : String -> Session -> Cmd Msgs.Msg
getPackageCmd pkgName session =
    getPackage pkgName session
        |> toCmd GetPackageCompleted Msgs.PackageManagementDetailMsg


getPackageCompleted : Model -> Result Jwt.JwtError PackageDetail -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok package ->
                    { model | package = Just package }

                Err error ->
                    { model | error = "Unable to get package" }
    in
    ( { newModel | loading = False }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackageCompleted result ->
            getPackageCompleted model result
