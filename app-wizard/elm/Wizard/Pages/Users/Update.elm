module Wizard.Pages.Users.Update exposing (fetchData, update)

import Random exposing (Seed)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Users.Create.Update
import Wizard.Pages.Users.Edit.Update
import Wizard.Pages.Users.Index.Update
import Wizard.Pages.Users.Models exposing (Model)
import Wizard.Pages.Users.Msgs exposing (Msg(..))
import Wizard.Pages.Users.Routes exposing (Route(..))


fetchData : Route -> AppState -> Cmd Msg
fetchData route appState =
    case route of
        EditRoute uuidOrCurrent subroute ->
            Cmd.map EditMsg <|
                Wizard.Pages.Users.Edit.Update.fetchData appState uuidOrCurrent subroute

        IndexRoute _ _ ->
            Cmd.map IndexMsg <|
                Wizard.Pages.Users.Index.Update.fetchData

        _ ->
            Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Seed, Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        CreateMsg createMsg ->
            let
                ( newSeed, createModel, cmd ) =
                    Wizard.Pages.Users.Create.Update.update createMsg (wrapMsg << CreateMsg) appState model.createModel
            in
            ( newSeed, { model | createModel = createModel }, cmd )

        EditMsg editMsg ->
            let
                ( editModel, cmd ) =
                    Wizard.Pages.Users.Edit.Update.update editMsg (wrapMsg << EditMsg) appState model.editModel
            in
            ( appState.seed, { model | editModel = editModel }, cmd )

        IndexMsg indexMsg ->
            let
                ( indexModel, cmd ) =
                    Wizard.Pages.Users.Index.Update.update indexMsg (wrapMsg << IndexMsg) appState model.indexModel
            in
            ( appState.seed, { model | indexModel = indexModel }, cmd )
