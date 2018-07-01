module Common.Questionnaire.Update exposing (..)

import Common.Models exposing (getServerError)
import Common.Questionnaire.Models exposing (..)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(FeedbackMsg), Msg(..))
import Common.Questionnaire.Requests exposing (postFeedback)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import FormEngine.Msgs
import FormEngine.Update exposing (updateForm)
import Http
import KMEditor.Common.Models.Entities exposing (Chapter)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg msg ->
            ( handleFormMsg msg model, Cmd.none )

        SetActiveChapter chapter ->
            ( handleSetActiveChapter chapter model, Cmd.none )

        CloseFeedback ->
            ( { model | feedback = Nothing }, Cmd.none )

        FeedbackFormMsg formMsg ->
            ( { model | feedbackForm = Form.update feedbackFormValidation formMsg model.feedbackForm }, Cmd.none )

        SendFeedbackForm ->
            handleSendFeedbackForm model

        PostFeedbackCompleted result ->
            ( handlePostFeedbackCompleted result model, Cmd.none )


handleFormMsg : FormEngine.Msgs.Msg CustomFormMessage -> Model -> Model
handleFormMsg msg model =
    case model.activeChapterForm of
        Just form ->
            case msg of
                FormEngine.Msgs.CustomQuestionMsg questionUuid customMsg ->
                    case customMsg of
                        FeedbackMsg ->
                            { model
                                | feedback = Just questionUuid
                                , feedbackForm = initEmptyFeedbackFrom
                                , sendingFeedback = Unset
                                , feedbackResult = Nothing
                            }

                _ ->
                    { model | activeChapterForm = Just <| updateForm msg form }

        _ ->
            model


handleSetActiveChapter : Chapter -> Model -> Model
handleSetActiveChapter chapter model =
    model
        |> updateReplies
        |> setActiveChapter chapter
        |> setActiveChapterForm


handleSendFeedbackForm : Model -> ( Model, Cmd Msg )
handleSendFeedbackForm model =
    let
        newFeedbackForm =
            Form.update feedbackFormValidation Form.Submit model.feedbackForm
    in
    case Form.getOutput newFeedbackForm of
        Just feedbackForm ->
            let
                cmd =
                    postFeedbackCmd feedbackForm (model.feedback |> Maybe.withDefault "") model.questionnaire.package.id
            in
            ( { model | feedbackForm = newFeedbackForm, sendingFeedback = Loading }, cmd )

        _ ->
            ( { model | feedbackForm = newFeedbackForm }, Cmd.none )


handlePostFeedbackCompleted : Result Http.Error Feedback -> Model -> Model
handlePostFeedbackCompleted result model =
    case result of
        Ok feedback ->
            { model
                | sendingFeedback = Success "Your feedback has been sent."
                , feedbackResult = Just feedback
            }

        Err error ->
            { model | sendingFeedback = getServerError error "Feedback could not be sent." }


postFeedbackCmd : FeedbackForm -> String -> String -> Cmd Msg
postFeedbackCmd feedbackFrom questionUuid packageId =
    feedbackFrom
        |> encodeFeedbackFrom questionUuid packageId
        |> postFeedback
        |> Http.send PostFeedbackCompleted
