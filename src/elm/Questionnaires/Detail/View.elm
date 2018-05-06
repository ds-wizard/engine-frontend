module Questionnaires.Detail.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.View exposing (fullPageActionResultView, pageHeader)
import FormEngine.Model exposing (Form)
import FormEngine.Msgs
import FormEngine.View exposing (viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KnowledgeModels.Editor.Models.Entities exposing (Chapter)
import List.Extra as List
import Msgs
import Questionnaires.Common.Models exposing (QuestionnaireDetail)
import Questionnaires.Detail.Models exposing (Model)
import Questionnaires.Detail.Msgs exposing (Msg(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    fullPageActionResultView (content wrapMsg model) model.questionnaire


content : (Msg -> Msgs.Msg) -> Model -> QuestionnaireDetail -> Html Msgs.Msg
content wrapMsg model questionnaire =
    div [ class "questionnaire-detail" ]
        [ pageHeader (questionnaire.name ++ " (" ++ questionnaire.package.name ++ ", " ++ questionnaire.package.version ++ ")") []
        , div [ class "row" ]
            [ div [ class "col-xs-12 col-md-3 col-lg-2" ] [ chapterList wrapMsg questionnaire model.activeChapter ]
            , div [ class "col-xs-11 col-md-8 col-lg 10" ]
                [ chapterHeader model.activeChapter
                , viewChapterForm model |> Html.map (wrapMsg << FormMsg)
                ]
            ]
        ]


viewChapterForm : Model -> Html FormEngine.Msgs.Msg
viewChapterForm model =
    case model.activeChapterForm of
        Just form ->
            viewForm form

        _ ->
            emptyNode


chapterList : (Msg -> Msgs.Msg) -> QuestionnaireDetail -> Maybe Chapter -> Html Msgs.Msg
chapterList wrapMsg questionnaire activeChapter =
    ul [ class "nav nav-pills nav-stacked" ]
        (List.map (chapterListChapter wrapMsg activeChapter) questionnaire.knowledgeModel.chapters)


chapterListChapter : (Msg -> Msgs.Msg) -> Maybe Chapter -> Chapter -> Html Msgs.Msg
chapterListChapter wrapMsg activeChapter chapter =
    li [ classList [ ( "active", activeChapter == Just chapter ) ] ]
        [ a [ onClick (wrapMsg <| SetActiveChapter chapter) ] [ text chapter.title ]
        ]


chapterHeader : Maybe Chapter -> Html Msgs.Msg
chapterHeader maybeChapter =
    case maybeChapter of
        Just chapter ->
            div []
                [ h3 [] [ text chapter.title ]
                , p [ class "chapter-description" ] [ text chapter.text ]
                ]

        _ ->
            emptyNode
