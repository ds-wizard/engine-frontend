module KMEditor.Editor.View.Breadcrumbs exposing (breadcrumbs)

import Html exposing (..)
import Html.Attributes exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import Msgs


breadcrumbs : KnowledgeModelEditor -> Html Msgs.Msg
breadcrumbs editor =
    ul [ class "breadcrumb" ]
        (List.map breadcrumbsElement <| getKnowledgeModelBreadcrumbs editor)


breadcrumbsElement : String -> Html Msgs.Msg
breadcrumbsElement name =
    li [ class "breadcrumb-item" ] [ text name ]


getKnowledgeModelBreadcrumbs : KnowledgeModelEditor -> List String
getKnowledgeModelBreadcrumbs ((KnowledgeModelEditor editor) as kme) =
    let
        items =
            getActiveChapterEditor editor.chapters
                |> Maybe.map getChapterBreadcrumbs
                |> Maybe.withDefault []
    in
    [ getKnowledgeModelEditorName kme ] ++ items


getChapterBreadcrumbs : ChapterEditor -> List String
getChapterBreadcrumbs ((ChapterEditor editor) as ce) =
    let
        items =
            getActiveQuestionEditor editor.questions
                |> Maybe.map getQuestionBreadcrumbs
                |> Maybe.withDefault []
    in
    [ getChapterEditorName ce ] ++ items


getQuestionBreadcrumbs : QuestionEditor -> List String
getQuestionBreadcrumbs ((QuestionEditor editor) as qe) =
    let
        activeChildren =
            ( getActiveAnswerEditor editor.answers
            , getActiveReferenceEditor editor.references
            , getActiveExpertEditor editor.experts
            )

        items =
            case activeChildren of
                ( Just ae, _, _ ) ->
                    getAnswerBreadcrumbs ae

                ( _, Just re, _ ) ->
                    getReferenceBreadcrumbs re

                ( _, _, Just ee ) ->
                    getExpertBreadcrumbs ee

                _ ->
                    []
    in
    [ getQuestionEditorName qe ] ++ items


getAnswerBreadcrumbs : AnswerEditor -> List String
getAnswerBreadcrumbs ((AnswerEditor editor) as ae) =
    let
        items =
            getActiveQuestionEditor editor.followUps
                |> Maybe.map getQuestionBreadcrumbs
                |> Maybe.withDefault []
    in
    [ getAnswerEditorName ae ] ++ items


getReferenceBreadcrumbs : ReferenceEditor -> List String
getReferenceBreadcrumbs =
    getReferenceEditorName >> List.singleton


getExpertBreadcrumbs : ExpertEditor -> List String
getExpertBreadcrumbs =
    getExpertEditorName >> List.singleton
