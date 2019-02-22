module KMEditor.Editor2.Preview.Update exposing (update)

import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import KMEditor.Editor2.Preview.Models exposing (..)
import KMEditor.Editor2.Preview.Msgs exposing (Msg(..))


update : Msg -> Model -> Model
update msg =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg

        AddTag uuid ->
            addTag uuid

        RemoveTag uuid ->
            removeTag uuid

        SelectAllTags ->
            selectAllTags

        SelectNoneTags ->
            selectNoneTags


handleQuestionnaireMsg : Common.Questionnaire.Msgs.Msg -> Model -> Model
handleQuestionnaireMsg msg model =
    let
        ( newQuestionnaireModel, _ ) =
            Common.Questionnaire.Update.update msg Nothing model.questionnaireModel
    in
    { model | questionnaireModel = newQuestionnaireModel }
