module Wizard.Users.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Users.Create.Update
import Wizard.Users.Edit.Update
import Wizard.Users.Index.Update
import Wizard.Users.Models exposing (Model)
import Wizard.Users.Msgs exposing (Msg(..))
import Wizard.Users.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        EditRoute uuidOrCurrent subroute ->
            Cmd.map EditMsg <|
                Wizard.Users.Edit.Update.fetchData appState uuidOrCurrent subroute

        IndexRoute _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Users.Index.Update.fetchData

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg createMsg ->
            let
                ( newSeed, createModel, cmd ) =
                    Wizard.Users.Create.Update.update createMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( newSeed, { model | createModel = createModel }, cmd )

        EditMsg editMsg ->
            let
                ( editModel, cmd ) =
                    Wizard.Users.Edit.Update.update editMsg (wrapMsg << EditMsg) appState model.editModel
            in
            ( appState.seed, { model | editModel = editModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Users.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
