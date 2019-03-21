module Common.Questionnaire.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (Session)
import ChartJS exposing (encodeChartConfig)
import Common.Models exposing (getServerError, getServerErrorJwt)
import Common.Questionnaire.Models exposing (..)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Common.Questionnaire.Requests exposing (getFeedbacks, postFeedback, postForSummaryReport)
import Form exposing (Form)
import FormEngine.Msgs
import FormEngine.Update exposing (updateForm)
import Http
import Jwt
import KMEditor.Common.Models.Entities exposing (Chapter)
import Ports
import Utils exposing (stringToInt)


update : Msg -> Maybe Session -> Model -> ( Model, Cmd Msg )
update msg maybeSession model =
    case msg of
        FormMsg formMsg ->
            handleFormMsg formMsg model

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
            case maybeSession of
                Just session ->
                    let
                        newModel =
                            updateReplies model
                    in
                    ( { newModel
                        | activePage = PageSummaryReport
                        , summaryReport = Loading
                      }
                    , postForSummaryReportCmd session newModel
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                    ( { model | summaryReport = getServerErrorJwt error "Unable to get summary report" }, Cmd.none )

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
                            , getFeedbacksCmd model.questionnaire.package.id questionUuid
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


postForSummaryReportCmd : Session -> Model -> Cmd Msg
postForSummaryReportCmd session model =
    encodeQuestionnaireDetail model.questionnaire
        |> postForSummaryReport session model.questionnaire.uuid
        |> Jwt.send PostForSummaryReportCompleted


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
