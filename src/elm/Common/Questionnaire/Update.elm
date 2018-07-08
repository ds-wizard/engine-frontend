module Common.Questionnaire.Update exposing (..)

import Common.Models exposing (getServerError)
import Common.Questionnaire.Models exposing (..)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(FeedbackMsg), Msg(..))
import Common.Questionnaire.Requests exposing (getFeedbacks, postFeedback)
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
            handleFormMsg msg model

        SetActiveChapter chapter ->
            ( handleSetActiveChapter chapter model, Cmd.none )

        CloseFeedback ->
            ( { model | feedback = Unset, feedbackQuestionUuid = Nothing }, Cmd.none )

        FeedbackFormMsg formMsg ->
            ( { model | feedbackForm = Form.update feedbackFormValidation formMsg model.feedbackForm }, Cmd.none )

        SendFeedbackForm ->
            handleSendFeedbackForm model

        PostFeedbackCompleted result ->
            ( handlePostFeedbackCompleted result model, Cmd.none )

        GetFeedbacksCompleted result ->
            case model.feedback of
                Loading ->
                    case result of
                        Ok feedback ->
                            ( { model | feedback = Success feedback }, Cmd.none )

                        Err error ->
                            ( { model | feedback = getServerError error "Unable to get feedback" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


handleFormMsg : FormEngine.Msgs.Msg CustomFormMessage -> Model -> ( Model, Cmd Msg )
handleFormMsg msg model =
    case model.activeChapterForm of
        Just form ->
            case msg of
                FormEngine.Msgs.CustomQuestionMsg questionUuid customMsg ->
                    case customMsg of
                        FeedbackMsg ->
                            ( { model
                                | feedback = Loading
                                , feedbackQuestionUuid = Just questionUuid
                                , feedbackForm = initEmptyFeedbackFrom
                                , sendingFeedback = Unset
                                , feedbackResult = Nothing
                              }
                            , getFeedbacksCmd model.questionnaire.package.id questionUuid
                            )

                _ ->
                    ( { model | activeChapterForm = Just <| updateForm msg form }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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
                    postFeedbackCmd feedbackForm (model.feedbackQuestionUuid |> Maybe.withDefault "") model.questionnaire.package.id
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


getFeedbacksCmd : String -> String -> Cmd Msg
getFeedbacksCmd packageId questionUuid =
    getFeedbacks packageId questionUuid
        |> Http.send GetFeedbacksCompleted
