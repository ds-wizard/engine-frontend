module Common.Questionnaire.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import ChartJS exposing (encodeChartConfig)
import Common.Api.Feedbacks as FeedbacksApi
import Common.Api.Questionnaires as QuestionnairesApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Questionnaire.Models exposing (..)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Form exposing (Form)
import FormEngine.Msgs
import FormEngine.Update exposing (updateForm)
import KMEditor.Common.Models.Entities exposing (Chapter)
import Ports
import Utils exposing (stringToInt)


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg appState model

        SetLevel level ->
            ( { model
                | questionnaire = setLevel model.questionnaire <| stringToInt level
                , dirty = True
              }
            , Cmd.none
            )

        SetActiveChapter chapter ->
            ( handleSetActiveChapter chapter model, Cmd.none )

        ViewSummaryReport ->
            let
                newModel =
                    updateReplies model

                body =
                    encodeQuestionnaireDetail newModel.questionnaire

                cmd =
                    QuestionnairesApi.fetchSummaryReport model.questionnaire.uuid body appState PostForSummaryReportCompleted
            in
            ( { newModel
                | activePage = PageSummaryReport
                , summaryReport = Loading
              }
            , cmd
            )

        PostForSummaryReportCompleted result ->
            case result of
                Ok summaryReport ->
                    let
                        cmds =
                            List.map
                                (Ports.drawMetricsChart
                                    << encodeChartConfig
                                    << createChartConfig model.metrics model.questionnaire.knowledgeModel.chapters
                                )
                                summaryReport.chapterReports
                    in
                    ( { model | summaryReport = Success summaryReport }
                    , Cmd.batch cmds
                    )

                Err error ->
                    ( { model | summaryReport = getServerError error "Unable to get summary report" }, Cmd.none )

        CloseFeedback ->
            ( { model | feedback = Unset, feedbackQuestionUuid = Nothing }, Cmd.none )

        FeedbackFormMsg formMsg ->
            ( { model | feedbackForm = Form.update feedbackFormValidation formMsg model.feedbackForm }, Cmd.none )

        SendFeedbackForm ->
            handleSendFeedbackForm appState model

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


handleFormMsg : FormEngine.Msgs.Msg CustomFormMessage -> AppState -> Model -> ( Model, Cmd Msg )
handleFormMsg msg appState model =
    case model.activePage of
        PageChapter chapter form ->
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
                            , FeedbacksApi.getFeedbacks model.questionnaire.package.id questionUuid appState GetFeedbacksCompleted
                            )

                _ ->
                    ( updateReplies
                        { model
                            | activePage = PageChapter chapter (updateForm msg form)
                            , dirty = True
                        }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )


handleSetActiveChapter : Chapter -> Model -> Model
handleSetActiveChapter chapter model =
    model
        |> updateReplies
        |> setActiveChapter chapter


handleSendFeedbackForm : AppState -> Model -> ( Model, Cmd Msg )
handleSendFeedbackForm appState model =
    let
        newFeedbackForm =
            Form.update feedbackFormValidation Form.Submit model.feedbackForm
    in
    case Form.getOutput newFeedbackForm of
        Just feedbackForm ->
            let
                body =
                    encodeFeedbackFrom (model.feedbackQuestionUuid |> Maybe.withDefault "") model.questionnaire.package.id feedbackForm

                cmd =
                    FeedbacksApi.postFeedback body appState PostFeedbackCompleted
            in
            ( { model | feedbackForm = newFeedbackForm, sendingFeedback = Loading }, cmd )

        _ ->
            ( { model | feedbackForm = newFeedbackForm }, Cmd.none )


handlePostFeedbackCompleted : Result ApiError Feedback -> Model -> Model
handlePostFeedbackCompleted result model =
    case result of
        Ok feedback ->
            { model
                | sendingFeedback = Success "Your feedback has been sent."
                , feedbackResult = Just feedback
            }

        Err error ->
            { model | sendingFeedback = getServerError error "Feedback could not be sent." }
