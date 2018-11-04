module KMPackages.Import.Update exposing (update)

-- import FileReader exposing (NativeFile)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Json.Decode as Decode
import Jwt
import KMPackages.Import.Models exposing (Model)
import KMPackages.Import.Msgs exposing (Msg(..))
import KMPackages.Requests exposing (importPackage)
import KMPackages.Routing
import Models exposing (State)
import Msgs
import Requests exposing (getResultCmd)
import Routing exposing (Route(..), cmdNavigate)


update : Msg -> (Msg -> Msgs.Msg) -> State -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg state model =
    case msg of
        DragEnter ->
            ( { model | dnd = model.dnd + 1 }, Cmd.none )

        DragLeave ->
            ( { model | dnd = model.dnd - 1 }, Cmd.none )

        -- Drop files ->
        --     ( { model | dnd = 0, files = files }, Cmd.none )
        -- FilesSelect files ->
        --     ( { model | files = files }, Cmd.none )
        -- Submit ->
        --     handleSubmit wrapMsg session model
        Cancel ->
            -- ( { model | files = [], importing = Unset }, Cmd.none )
            ( { model | importing = Unset }, Cmd.none )

        ImportPackageCompleted result ->
            importPackageCompleted state model result

        _ ->
            ( model, Cmd.none )



-- handleSubmit : (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
-- handleSubmit wrapMsg session model =
--     case List.head model.files of
--         Just file ->
--             ( { model | importing = Loading }, importPackageCmd wrapMsg file session )
--         Nothing ->
--             ( model, Cmd.none )
-- importPackageCmd : (Msg -> Msgs.Msg) -> NativeFile -> Session -> Cmd Msgs.Msg
-- importPackageCmd wrapMsg file session =
--     importPackage file session
--         |> Jwt.send ImportPackageCompleted
--         |> Cmd.map wrapMsg


importPackageCompleted : State -> Model -> Result Jwt.JwtError Decode.Value -> ( Model, Cmd Msgs.Msg )
importPackageCompleted state model result =
    case result of
        Ok msg ->
            ( model, cmdNavigate state.key (KMPackages KMPackages.Routing.Index) )

        Err error ->
            ( { model | importing = getServerErrorJwt error "Importing package failed." }
            , getResultCmd result
            )
