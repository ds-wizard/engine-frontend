module KMEditor.Editor.Preview.Update exposing (update)

import Common.AppState exposing (AppState)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import KMEditor.Editor.Preview.Models exposing (..)
import KMEditor.Editor.Preview.Msgs exposing (Msg(..))


update : Msg -> AppState -> Model -> Model
update msg appState =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState

        AddTag uuid ->
            addTag uuid

        RemoveTag uuid ->
            removeTag uuid

        SelectAllTags ->
            selectAllTags

        SelectNoneTags ->
            selectNoneTags


handleQuestionnaireMsg : Common.Questionnaire.Msgs.Msg -> AppState -> Model -> Model
handleQuestionnaireMsg msg appState model =
    let
        ( newQuestionnaireModel, _ ) =
            Common.Questionnaire.Update.update msg appState model.questionnaireModel
    in
    { model | questionnaireModel = newQuestionnaireModel }
