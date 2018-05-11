module KMPackages.Index.Update exposing (getPackagesCmd, update)

{-|

@docs update, getPackagesCmd

-}

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMPackages.Index.Models exposing (Model)
import KMPackages.Index.Msgs exposing (Msg(..))
import KMPackages.Models exposing (Package)
import KMPackages.Requests exposing (getPackagesUnique)
import Msgs
import Requests exposing (toCmd)


{-| -}
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


{-| -}
update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetPackagesCompleted result ->
            getPackagesCompleted model result
