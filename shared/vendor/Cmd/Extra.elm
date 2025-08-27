module Cmd.Extra exposing (withNoCmd)


withNoCmd : model -> ( model, Cmd msg )
withNoCmd model =
    ( model, Cmd.none )
