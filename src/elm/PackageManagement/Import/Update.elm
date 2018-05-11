module PackageManagement.Import.Update exposing (update)

{-|

@docs update

-}

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import FileReader exposing (NativeFile)
import Json.Decode as Decode
import Jwt
import Msgs
import PackageManagement.Import.Models exposing (Model)
import PackageManagement.Import.Msgs exposing (Msg(..))
import PackageManagement.Requests exposing (importPackage)
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)


importPackageCmd : NativeFile -> Session -> Cmd Msgs.Msg
importPackageCmd file session =
    importPackage file session
        |> toCmd ImportPackageCompleted Msgs.PackageManagementImportMsg


handleSubmit : Session -> Model -> ( Model, Cmd Msgs.Msg )
handleSubmit session model =
    case List.head model.files of
        Just file ->
            ( { model | importing = Loading }, importPackageCmd file session )

        Nothing ->
            ( model, Cmd.none )


importPackageCompleted : Model -> Result Jwt.JwtError Decode.Value -> ( Model, Cmd Msgs.Msg )
importPackageCompleted model result =
    case result of
        Ok msg ->
            ( model, cmdNavigate PackageManagement )

        Err error ->
            ( { model | importing = getServerErrorJwt error "Importing package failed." }, Cmd.none )


{-| -}
update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        DragEnter ->
            ( { model | dnd = model.dnd + 1 }, Cmd.none )

        DragLeave ->
            ( { model | dnd = model.dnd - 1 }, Cmd.none )

        Drop files ->
            ( { model | dnd = 0, files = files }, Cmd.none )

        FilesSelect files ->
            ( { model | files = files }, Cmd.none )

        Submit ->
            handleSubmit session model

        Cancel ->
            ( { model | files = [], importing = Unset }, Cmd.none )

        ImportPackageCompleted result ->
            importPackageCompleted model result

        _ ->
            ( model, Cmd.none )
