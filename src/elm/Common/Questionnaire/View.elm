module Common.Questionnaire.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Questionnaire.Models exposing (FeedbackForm, FormExtraData, Model)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(FeedbackMsg), Msg(..))
import Common.Types exposing (ActionResult(Success))
import Common.View exposing (modalView)
import Common.View.Forms exposing (inputGroup, textAreaGroup)
import FormEngine.View exposing (FormViewConfig, viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Chapter)


viewQuestionnaire : Model -> Html Msg
viewQuestionnaire model =
    div [ class "Questionnaire row" ]
        [ div [ class "col-sm-12 col-md-3 col-lg-3 col-xl-3" ]
            [ chapterList model ]
        , div [ class "col-sm-11 col-md-8 col-lg-8 col-xl-7" ]
            [ chapterHeader model.activeChapter
            , viewChapterForm model
            ]
        , feedbackModal model
        ]


chapterList : Model -> Html Msg
chapterList model =
    div [ class "nav nav-pills flex-column" ]
        (List.map (chapterListChapter model.activeChapter) model.questionnaire.knowledgeModel.chapters)


chapterListChapter : Maybe Chapter -> Chapter -> Html Msg
chapterListChapter activeChapter chapter =
    a
        [ classList [ ( "nav-link", True ), ( "active", activeChapter == Just chapter ) ]
        , onClick <| SetActiveChapter chapter
        ]
        [ text chapter.title ]


chapterHeader : Maybe Chapter -> Html Msg
chapterHeader maybeChapter =
    case maybeChapter of
        Just chapter ->
            div []
                [ h3 [] [ text chapter.title ]
                , p [ class "chapter-description" ] [ text chapter.text ]
                ]

        _ ->
            emptyNode


viewExtraData : FormExtraData -> Html msg
viewExtraData data =
    case data.shortUuid of
        Just uuid ->
            p [ class "extra-data" ]
                [ a [ href <| "/book-references/" ++ uuid, target "_blank" ]
                    [ i [ class "fa fa-book" ] []
                    , text <| "Book Reference #" ++ uuid
                    ]
                ]

        Nothing ->
            emptyNode


formConfig : FormViewConfig CustomFormMessage FormExtraData
formConfig =
    { customActions = [ ( "fa-exclamation-circle", FeedbackMsg ) ]
    , viewExtraData = Just viewExtraData
    }


viewChapterForm : Model -> Html Msg
viewChapterForm model =
    case model.activeChapterForm of
        Just form ->
            viewForm formConfig form |> Html.map FormMsg

        _ ->
            emptyNode


feedbackModal : Model -> Html Msg
feedbackModal model =
    let
        visible =
            case model.feedback of
                Just _ ->
                    True

                Nothing ->
                    False

        modalContent =
            case model.sendingFeedback of
                Success _ ->
                    case model.feedbackResult of
                        Just feedback ->
                            [ p []
                                [ text "You can follow the GitHub "
                                , a [ href <| "https://github.com/DSWGlobal/dsw-staging/issues/" ++ toString feedback.issueId, target "_blank" ]
                                    [ text <| "issue " ++ toString feedback.issueId ]
                                , text "."
                                ]
                            ]

                        Nothing ->
                            [ emptyNode ]

                _ ->
                    [ div [ class "alert alert-info" ]
                        [ text "If you found something wrong with the question, you can send your feedback how to improve it." ]
                    , inputGroup model.feedbackForm "title" "Title" |> Html.map FeedbackFormMsg
                    , textAreaGroup model.feedbackForm "content" "Description" |> Html.map FeedbackFormMsg
                    ]

        ( actionName, actionMsg ) =
            case model.sendingFeedback of
                Success _ ->
                    ( "Done", CloseFeedback )

                _ ->
                    ( "Send", SendFeedbackForm )

        modalConfig =
            { modalTitle = "Feedback"
            , modalContent = modalContent
            , visible = visible
            , actionResult = model.sendingFeedback
            , actionName = actionName
            , actionMsg = actionMsg
            , cancelMsg = CloseFeedback
            }
    in
    modalView modalConfig
