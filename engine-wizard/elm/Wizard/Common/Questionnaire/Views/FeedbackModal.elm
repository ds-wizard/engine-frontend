module Wizard.Common.Questionnaire.Views.FeedbackModal exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lf, lh, lx)
import String exposing (fromInt)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model)
import Wizard.Common.Questionnaire.Models.Feedback exposing (Feedback)
import Wizard.Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Questionnaire.Views.FeedbackModal"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.Questionnaire.Views.FeedbackModal"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Common.Questionnaire.Views.FeedbackModal"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Questionnaire.Views.FeedbackModal"


view : AppState -> Model -> Html Msg
view appState model =
    let
        visible =
            case model.feedback of
                Unset ->
                    False

                _ ->
                    True

        modalContent =
            case model.sendingFeedback of
                Success _ ->
                    case model.feedbackResult of
                        Just feedback ->
                            let
                                issueLink =
                                    a [ href feedback.issueUrl, target "_blank" ]
                                        [ text <| lf_ "issue" [ fromInt feedback.issueId ] appState ]
                            in
                            [ p []
                                (lh_ "follow" [ issueLink ] appState)
                            ]

                        Nothing ->
                            [ emptyNode ]

                _ ->
                    feedbackModalContent appState model

        ( actionName, actionMsg, cancelMsg ) =
            case model.sendingFeedback of
                Success _ ->
                    ( l_ "done" appState, CloseFeedback, Nothing )

                _ ->
                    ( l_ "send" appState, SendFeedbackForm, Just <| CloseFeedback )

        modalConfig =
            { modalTitle = l_ "title" appState
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.sendingFeedback
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = cancelMsg
            , dangerous = False
            }
    in
    Modal.confirm appState modalConfig


feedbackModalContent : AppState -> Model -> List (Html Msg)
feedbackModalContent appState model =
    let
        feedbackList =
            case model.feedback of
                Success feedbacks ->
                    if List.length feedbacks > 0 then
                        div []
                            [ div []
                                [ lx_ "reportedIssues" appState ]
                            , ul [] (List.map feedbackIssue feedbacks)
                            ]

                    else
                        emptyNode

                _ ->
                    emptyNode
    in
    [ div [ class "alert alert-info" ]
        [ lx_ "info" appState ]
    , feedbackList
    , FormGroup.input appState model.feedbackForm "title" (l_ "form.title" appState) |> Html.map FeedbackFormMsg
    , FormGroup.textarea appState model.feedbackForm "content" (l_ "form.description" appState) |> Html.map FeedbackFormMsg
    ]


feedbackIssue : Feedback -> Html Msg
feedbackIssue feedback =
    li []
        [ a [ href feedback.issueUrl, target "_blank" ]
            [ text feedback.title ]
        ]
