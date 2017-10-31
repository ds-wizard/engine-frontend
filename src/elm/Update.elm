module Update exposing (..)

import Auth.Update
import Models exposing (Model)
import Msgs exposing (Msg)
import Navigation
import Routing exposing (parseLocation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.ChangeLocation path ->
            ( model, Navigation.newUrl path )

        Msgs.OnLocationChange location ->
            ( { model | route = parseLocation location }, Cmd.none ) |> Debug.log "OnLocationChange"

        Msgs.AuthMsg msg ->
            let
                ( authModel, cmd ) =
                    Auth.Update.update msg model.authModel
            in
            ( { model | authModel = authModel }, cmd )
