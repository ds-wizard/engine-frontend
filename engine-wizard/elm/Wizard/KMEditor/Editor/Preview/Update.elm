module Wizard.KMEditor.Editor.Preview.Update exposing (update)

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.KMEditor.Editor.Preview.Models exposing (..)
import Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))


update : Msg -> AppState -> Model -> ( Seed, Model, Cmd Msg )
update msg appState model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState model

        AddTag uuid ->
            ( appState.seed, addTag uuid model, Cmd.none )

        RemoveTag uuid ->
            ( appState.seed, removeTag uuid model, Cmd.none )

        SelectAllTags ->
            ( appState.seed, selectAllTags model, Cmd.none )

        SelectNoneTags ->
            ( appState.seed, selectNoneTags model, Cmd.none )


handleQuestionnaireMsg : Questionnaire.Msg -> AppState -> Model -> ( Seed, Model, Cmd Msg )
handleQuestionnaireMsg msg appState model =
    let
        ( newSeed, newQuestionnaireModel, cmd ) =
            Questionnaire.update msg
                QuestionnaireMsg
                Nothing
                appState
                { levels = model.levels
                , metrics = model.metrics
                , events = model.events
                }
                model.questionnaireModel
    in
    ( newSeed
    , { model | questionnaireModel = newQuestionnaireModel }
    , cmd
    )
