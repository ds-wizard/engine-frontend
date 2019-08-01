module Common.Questionnaire.Views.FeedbackModal exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (emptyNode)
import Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model)
import Common.Questionnaire.Models.Feedback exposing (Feedback)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Common.View.FormGroup as FormGroup
import Common.View.Modal as Modal
import Html exposing (..)
import Html.Attributes exposing (..)
import String exposing (fromInt)


view : Model -> Html Msg
view model =
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
                            [ p []
                                [ text "You can follow the GitHub "
                                , a [ href feedback.issueUrl, target "_blank" ]
                                    [ text <| "issue " ++ fromInt feedback.issueId ]
                                , text "."
                                ]
                            ]

                        Nothing ->
                            [ emptyNode ]

                _ ->
                    feedbackModalContent model

        ( actionName, actionMsg, cancelMsg ) =
            case model.sendingFeedback of
                Success _ ->
                    ( "Done", CloseFeedback, Nothing )

                _ ->
                    ( "Send", SendFeedbackForm, Just <| CloseFeedback )

        modalConfig =
            { modalTitle = "Feedback"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.sendingFeedback
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = cancelMsg
            , dangerous = False
            }
    in
    Modal.confirm modalConfig


feedbackModalContent : Model -> List (Html Msg)
feedbackModalContent model =
    let
        feedbackList =
            case model.feedback of
                Success feedbacks ->
                    if List.length feedbacks > 0 then
                        div []
                            [ div []
                                [ text "There are already some issues reported with this question" ]
                            , ul [] (List.map feedbackIssue feedbacks)
                            ]

                    else
                        emptyNode

                _ ->
                    emptyNode
    in
    [ div [ class "alert alert-info" ]
        [ text "If you found something wrong with the question, you can send us your feedback how to improve it." ]
    , feedbackList
    , FormGroup.input model.feedbackForm "title" "Title" |> Html.map FeedbackFormMsg
    , FormGroup.textarea model.feedbackForm "content" "Description" |> Html.map FeedbackFormMsg
    ]


feedbackIssue : Feedback -> Html Msg
feedbackIssue feedback =
    li []
        [ a [ href feedback.issueUrl, target "_blank" ]
            [ text feedback.title ]
        ]
