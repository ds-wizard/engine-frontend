module Wizard.Common.Components.Questionnaire.FeedbackModal exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

--import Shared.Locale exposing (l, lf, lg, lh, lx)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, p, text, ul)
import Html.Attributes exposing (class, href, target)
import Maybe.Extra as Maybe
import Shared.Api.Feedbacks as FeedbacksApi
import Shared.Data.Feedback exposing (Feedback)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode)
import String exposing (fromInt)
import String.Format as String
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Questionnaire.FeedbackForm as FeedbackForm exposing (FeedbackForm)
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal



--l_ : String -> AppState -> String
--l_ =
--    l "Wizard.Common.Components.Questionnaire.FeedbackModal"
--
--
--lf_ : String -> List String -> AppState -> String
--lf_ =
--    lf "Wizard.Common.Components.Questionnaire.FeedbackModal"
--
--
--lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
--lh_ =
--    lh "Wizard.Common.Components.Questionnaire.FeedbackModal"
--
--
--lx_ : String -> AppState -> Html msg
--lx_ =
--    lx "Wizard.Common.Components.Questionnaire.FeedbackModal"


type alias Model =
    { target : Maybe ( String, String )
    , feedbacks : ActionResult (List Feedback)
    , feedbackResult : ActionResult Feedback
    , feedbackForm : Form FormError FeedbackForm
    }


init : Model
init =
    { target = Nothing
    , feedbacks = Unset
    , feedbackResult = Unset
    , feedbackForm = FeedbackForm.initEmpty
    }


type Msg
    = OpenFeedback String String
    | CloseFeedback
    | GetFeedbacksComplete (Result ApiError (List Feedback))
    | PostFeedbackComplete (Result ApiError Feedback)
    | FeedbackFormMsg Form.Msg


update : Msg -> AppState -> Model -> ( Model, Cmd Msg )
update msg appState model =
    case msg of
        OpenFeedback packageId questionUuid ->
            let
                newModel =
                    { model
                        | target = Just ( packageId, questionUuid )
                        , feedbacks = Loading
                        , feedbackForm = FeedbackForm.initEmpty
                        , feedbackResult = Unset
                    }

                cmd =
                    FeedbacksApi.getFeedbacks packageId questionUuid appState GetFeedbacksComplete
            in
            ( newModel, cmd )

        CloseFeedback ->
            ( { model | feedbacks = Unset, target = Nothing }, Cmd.none )

        GetFeedbacksComplete result ->
            case model.feedbacks of
                Loading ->
                    case result of
                        Ok feedbacks ->
                            ( { model | feedbacks = Success feedbacks }, Cmd.none )

                        Err error ->
                            ( { model | feedbacks = ApiError.toActionResult appState (gettext "Unable to get feedback." appState.locale) error }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        PostFeedbackComplete result ->
            case result of
                Ok feedbackResult ->
                    ( { model | feedbackResult = Success feedbackResult }, Cmd.none )

                Err error ->
                    ( { model | feedbackResult = ApiError.toActionResult appState (gettext "Feedback could not be sent." appState.locale) error }
                    , Cmd.none
                    )

        FeedbackFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.feedbackForm, model.target ) of
                ( Form.Submit, Just feedbackForm, Just ( packageId, questionUuid ) ) ->
                    let
                        body =
                            FeedbackForm.encode questionUuid packageId feedbackForm

                        cmd =
                            FeedbacksApi.postFeedback body appState PostFeedbackComplete
                    in
                    ( { model | feedbackResult = Loading }, cmd )

                _ ->
                    ( { model | feedbackForm = Form.update FeedbackForm.validation formMsg model.feedbackForm }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    let
        visible =
            Maybe.isJust model.target

        modalContent =
            case model.feedbackResult of
                Success feedback ->
                    let
                        issueLink =
                            a [ href feedback.issueUrl, target "_blank" ]
                                [ text <| String.format (gettext "Issue %s" appState.locale) [ fromInt feedback.issueId ] ]
                    in
                    [ p []
                        (String.formatHtml (gettext "You can follow the GitHub %s." appState.locale) [ issueLink ])
                    ]

                _ ->
                    feedbackModalContent appState model

        ( actionName, actionMsg, cancelMsg ) =
            case model.feedbackResult of
                Success _ ->
                    ( gettext "Done" appState.locale, CloseFeedback, Nothing )

                _ ->
                    ( gettext "Send" appState.locale, FeedbackFormMsg Form.Submit, Just <| CloseFeedback )

        modalConfig =
            { modalTitle = gettext "Title" appState.locale
            , modalContent = modalContent
            , visible = visible
            , actionResult = ActionResult.map (\_ -> gettext "Your feedback has been sent." appState.locale) model.feedbackResult
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = cancelMsg
            , dangerous = False
            , dataCy = "questionnaire-feedback"
            }
    in
    Modal.confirm appState modalConfig


feedbackModalContent : AppState -> Model -> List (Html Msg)
feedbackModalContent appState model =
    let
        feedbackList =
            case model.feedbacks of
                Success feedbacks ->
                    if List.length feedbacks > 0 then
                        div []
                            [ div []
                                [ text (gettext "There are already some issues reported with this question." appState.locale) ]
                            , ul [] (List.map feedbackIssue feedbacks)
                            ]

                    else
                        emptyNode

                _ ->
                    emptyNode
    in
    [ div [ class "alert alert-info" ]
        [ text (gettext "If you found something wrong with the question, you can send us your recommendation on how to improve it." appState.locale) ]
    , feedbackList
    , FormGroup.input appState model.feedbackForm "title" (gettext "Title" appState.locale) |> Html.map FeedbackFormMsg
    , FormGroup.textarea appState model.feedbackForm "content" (gettext "Description" appState.locale) |> Html.map FeedbackFormMsg
    ]


feedbackIssue : Feedback -> Html Msg
feedbackIssue feedback =
    li []
        [ a [ href feedback.issueUrl, target "_blank" ]
            [ text feedback.title ]
        ]
