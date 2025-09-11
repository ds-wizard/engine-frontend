module Common.Utils.CmdUtils exposing (withNoCmd)


withNoCmd : model -> ( model, Cmd msg )
withNoCmd model =
    ( model, Cmd.none )
