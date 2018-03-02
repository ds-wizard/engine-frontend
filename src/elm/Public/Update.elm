module Public.Update exposing (..)

import Msgs
import Public.Login.Update
import Public.Models exposing (Model)
import Public.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        LoginMsg msg ->
            let
                ( loginModel, cmd ) =
                    Public.Login.Update.update msg (wrapMsg << LoginMsg) model.loginModel
            in
            ( { model | loginModel = loginModel }, cmd )

        _ ->
            ( model, Cmd.none )
