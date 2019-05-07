module KMEditor.Editor.Preview.Update exposing (update)

import Common.AppState exposing (AppState)
import Common.Questionnaire.Msgs
import Common.Questionnaire.Update
import KMEditor.Editor.Preview.Models exposing (..)
import KMEditor.Editor.Preview.Msgs exposing (Msg(..))


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        QuestionnaireMsg questionnaireMsg ->
            handleQuestionnaireMsg questionnaireMsg appState model

        AddTag uuid ->
            ( addTag uuid model, Cmd.none )

        RemoveTag uuid ->
            ( removeTag uuid model, Cmd.none )

        SelectAllTags ->
            ( selectAllTags model, Cmd.none )

        SelectNoneTags ->
            ( selectNoneTags model, Cmd.none )


handleQuestionnaireMsg : Common.Questionnaire.Msgs.Msg -> AppState -> Model -> ( Model, Cmd Msg )
handleQuestionnaireMsg msg appState model =
    let
        ( newQuestionnaireModel, cmd ) =
            Common.Questionnaire.Update.update msg appState model.questionnaireModel
    in
    ( { model | questionnaireModel = newQuestionnaireModel }
    , Cmd.map QuestionnaireMsg cmd
    )
