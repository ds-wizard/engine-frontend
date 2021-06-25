module Wizard.Projects.Create.Update exposing
    ( fetchData
    , update
    )

import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Projects.Create.CustomCreate.Update as CustomCreateUpdate
import Wizard.Projects.Create.Models exposing (CreateModel(..), Model)
import Wizard.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Projects.Create.TemplateCreate.Update as TemplateCreateUpdate


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    case model.createModel of
        CustomCreateModel customCreateModel ->
            Cmd.map CustomCreateMsg <|
                CustomCreateUpdate.fetchData appState customCreateModel

        TemplateCreateModel templateCreateModel ->
            Cmd.map TemplateCreateMsg <|
                TemplateCreateUpdate.fetchData appState templateCreateModel


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case ( msg, model.createModel ) of
        ( CustomCreateMsg customCreateMsg, CustomCreateModel customCreateModel ) ->
            let
                ( newCustomCreateModel, customCreateCmd ) =
                    CustomCreateUpdate.update (wrapMsg << CustomCreateMsg) customCreateMsg appState customCreateModel
            in
            ( { model | createModel = CustomCreateModel newCustomCreateModel }
            , customCreateCmd
            )

        ( TemplateCreateMsg templateCreateMsg, TemplateCreateModel templateCreateModel ) ->
            let
                ( newTemplateCreateModel, templateCreateCmd ) =
                    TemplateCreateUpdate.update (wrapMsg << TemplateCreateMsg) templateCreateMsg appState templateCreateModel
            in
            ( { model | createModel = TemplateCreateModel newTemplateCreateModel }
            , templateCreateCmd
            )

        _ ->
            ( model, Cmd.none )
