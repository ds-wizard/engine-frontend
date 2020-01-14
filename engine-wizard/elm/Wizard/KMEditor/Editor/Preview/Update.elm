module Wizard.KMEditor.Editor.Preview.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Questionnaire.Msgs
import Wizard.Common.Questionnaire.Update
import Wizard.KMEditor.Editor.Preview.Models exposing (..)
import Wizard.KMEditor.Editor.Preview.Msgs exposing (Msg(..))


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState model

        AddTag uuid ->
            ( addTag appState uuid model, Cmd.none )

        RemoveTag uuid ->
            ( removeTag appState uuid model, Cmd.none )

        SelectAllTags ->
            ( selectAllTags appState model, Cmd.none )

        SelectNoneTags ->
            ( selectNoneTags appState model, Cmd.none )


handleQuestionnaireMsg : Wizard.Common.Questionnaire.Msgs.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleQuestionnaireMsg msg appState model =
    let
        ( newQuestionnaireModel, cmd ) =
            Wizard.Common.Questionnaire.Update.update msg appState model.questionnaireModel
    in
    ( { model | questionnaireModel = newQuestionnaireModel }
    , Cmd.map QuestionnaireMsg cmd
    )
