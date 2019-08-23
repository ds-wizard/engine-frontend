module Users.Update exposing (fetchData, update)

import Common.AppState exposing (AppState)
import Msgs
import Random exposing (Seed)
import Users.Create.Update
import Users.Edit.Update
import Users.Index.Update
import Users.Models exposing (Model)
import Users.Msgs exposing (Msg(..))
import Users.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        EditRoute uuid ->
            Cmd.map EditMsg <|
                Users.Edit.Update.fetchData appState uuid

        IndexRoute ->
            Cmd.map IndexMsg <|
                Users.Index.Update.fetchData appState

        _ ->
            Cmd.none


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg createMsg ->
            let
                ( newSeed, createModel, cmd ) =
                    Users.Create.Update.update createMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( newSeed, { model | createModel = createModel }, cmd )

        EditMsg editMsg ->
            let
                ( editModel, cmd ) =
                    Users.Edit.Update.update editMsg (wrapMsg << EditMsg) appState model.editModel
            in
            ( appState.seed, { model | editModel = editModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Users.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
