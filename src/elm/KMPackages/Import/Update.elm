module KMPackages.Import.Update exposing (update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import FileReader exposing (NativeFile)
import Json.Decode as Decode
import Jwt
import KMPackages.Import.Models exposing (Model)
import KMPackages.Import.Msgs exposing (Msg(..))
import KMPackages.Requests exposing (importPackage)
import KMPackages.Routing
import Msgs
import Routing exposing (Route(..), cmdNavigate)


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
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
            handleSubmit wrapMsg session model

        Cancel ->
            ( { model | files = [], importing = Unset }, Cmd.none )

        ImportPackageCompleted result ->
            importPackageCompleted model result

        _ ->
            ( model, Cmd.none )


handleSubmit : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleSubmit wrapMsg session model =
    case List.head model.files of
        Just file ->
            ( { model | importing = Loading }, importPackageCmd wrapMsg file session )

        Nothing ->
            ( model, Cmd.none )


importPackageCmd : (Msg -> Msgs.Msg) -> NativeFile -> Session -> Cmd Msgs.Msg
importPackageCmd wrapMsg file session =
    importPackage file session
        |> Jwt.send ImportPackageCompleted
        |> Cmd.map wrapMsg


importPackageCompleted : Model -> Result Jwt.JwtError Decode.Value -> ( Model, Cmd Msgs.Msg )
importPackageCompleted model result =
    case result of
        Ok msg ->
            ( model, cmdNavigate (KMPackages KMPackages.Routing.Index) )

        Err error ->
            ( { model | importing = getServerErrorJwt error "Importing package failed." }, Cmd.none )
