module Common.Questionnaire.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.Questionnaire.Models exposing (Model)
import Common.Questionnaire.Msgs exposing (Msg(..))
import FormEngine.View exposing (viewForm)
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


viewChapterForm : Model -> Html Msg
viewChapterForm model =
    case model.activeChapterForm of
        Just form ->
            viewForm form |> Html.map FormMsg

        _ ->
            emptyNode
