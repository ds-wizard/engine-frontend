module PackageManagement.Index.Update exposing (..)

import Auth.Models exposing (Session)
import Jwt
import Msgs
import PackageManagement.Index.Models exposing (Model)
import PackageManagement.Index.Msgs exposing (Msg(..))
import PackageManagement.Models exposing (Package)
import PackageManagement.Requests exposing (getPackagesUnique)
import Requests exposing (toCmd)


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
                    { model | packages = packages }

                Err error ->
                    { model | error = "Unable to fetch package list" }
    in
    ( { newModel | loading = False }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result
