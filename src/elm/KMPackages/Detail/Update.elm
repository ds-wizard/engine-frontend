module KMPackages.Detail.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Bootstrap.Dropdown as Dropdown
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Jwt
import KMPackages.Common.Models exposing (PackageDetail)
import KMPackages.Detail.Models exposing (..)
import KMPackages.Detail.Msgs exposing (Msg(..))
import KMPackages.Requests exposing (..)
import KMPackages.Routing exposing (Route(..))
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)


fetchData : (Msg -> Msgs.Msg) -> String -> String -> Session -> Cmd Msgs.Msg
fetchData wrapMsg organizationId kmId session =
    getPackagesFiltered organizationId kmId session
        |> Jwt.send GetPackageCompleted
        |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        GetPackageCompleted result ->
            getPackageCompleted model result

        ShowHideDeleteVersion version ->
            ( { model | versionToBeDeleted = version, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg session model

        DeleteVersionCompleted result ->
            deleteVersionCompleted model result

        DropdownMsg packageDetail state ->
            handleDropdownToggle model packageDetail state


getPackageCompleted : Model -> Result Jwt.JwtError (List PackageDetail) -> ( Model, Cmd Msgs.Msg )
getPackageCompleted model result =
    let
        newModel =
            case result of
                Ok packages ->
                    { model | packages = Success <| List.map initPackageDetailRow packages }

                Err error ->
                    { model | packages = getServerErrorJwt error "Unable to get package detail" }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handleDeleteVersion : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleDeleteVersion wrapMsg session model =
    case ( currentPackage model, model.versionToBeDeleted ) of
        ( Just package, Just version ) ->
            ( { model | deletingVersion = Loading }
            , deletePackageVersionCmd wrapMsg version session
            )

        _ ->
            ( model, Cmd.none )


deletePackageVersionCmd : (Msg -> Msgs.Msg) -> String -> Session -> Cmd Msgs.Msg
deletePackageVersionCmd wrapMsg packageId session =
    deletePackageVersion packageId session
        |> Jwt.send DeleteVersionCompleted
        |> Cmd.map wrapMsg


deleteVersionCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
deleteVersionCompleted model result =
    case result of
        Ok version ->
            let
                route =
                    case ( packagesLength model > 1, currentPackage model ) of
                        ( True, Just package ) ->
                            KMPackages <| Detail package.organizationId package.kmId

                        _ ->
                            KMPackages Index
            in
            ( model, cmdNavigate route )

        Err error ->
            ( { model
                | deletingVersion = getServerErrorJwt error "Version could not be deleted"
              }
            , getResultCmd result
            )


handleDropdownToggle : Model -> PackageDetail -> Dropdown.State -> ( Model, Cmd Msgs.Msg )
handleDropdownToggle model packageDetail state =
    case model.packages of
        Success packageDetailRows ->
            let
                replaceWith row =
                    if row.packageDetail == packageDetail then
                        { row | dropdownState = state }
                    else
                        row

                newRows =
                    List.map replaceWith packageDetailRows
            in
            ( { model | packages = Success newRows }, Cmd.none )

        _ ->
            ( model, Cmd.none )
