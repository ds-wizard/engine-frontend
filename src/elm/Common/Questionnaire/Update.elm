module Common.Questionnaire.Update exposing (..)

import Common.Questionnaire.Models exposing (..)
import Common.Questionnaire.Msgs exposing (Msg(FormMsg, SetActiveChapter))
import FormEngine.Model exposing (getFormValues)
import FormEngine.Msgs
import FormEngine.Update exposing (updateForm)
import KMEditor.Common.Models.Entities exposing (Chapter)


update : Msg -> Model -> Model
update msg model =
    case msg of
        FormMsg msg ->
            handleFormMsg msg model

        SetActiveChapter chapter ->
            handleSetActiveChapter chapter model


handleFormMsg : FormEngine.Msgs.Msg -> Model -> Model
handleFormMsg msg model =
    case model.activeChapterForm of
        Just form ->
            { model | activeChapterForm = Just <| updateForm msg form }

        _ ->
            model


handleSetActiveChapter : Chapter -> Model -> Model
handleSetActiveChapter chapter model =
    model
        |> updateReplies
        |> setActiveChapter chapter
        |> setActiveChapterForm
