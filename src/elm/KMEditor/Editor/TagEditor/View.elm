module KMEditor.Editor.TagEditor.View exposing (view)

import Common.Html exposing (fa)
import Common.View.Flash as Flash
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import KMEditor.Editor.TagEditor.Models exposing (Model, hasQuestionTag)
import KMEditor.Editor.TagEditor.Msgs exposing (Msg(..))
import Msgs
import Utils exposing (getContrastColorHex)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    let
        content =
            if List.length model.knowledgeModel.tagUuids > 0 then
                if (List.length <| KnowledgeModel.getAllQuestions model.knowledgeModel) > 0 then
                    tagEditorTable model

                else
                    Flash.info "There are no questions, create them first in the Knowledge Model editor."

            else
                Flash.info "There are no tags, create them first in the Knowledge Model editor."
    in
    div [ class "KMEditor__Editor__TagEditor" ]
        [ content ]
        |> Html.map wrapMsg


tagEditorTable : Model -> Html Msg
tagEditorTable model =
    let
        tags =
            KnowledgeModel.getTags model.knowledgeModel
    in
    div [ class "editor-table-container" ]
        [ table []
            [ thead []
                [ tr []
                    ([ th [ class "top-left" ]
                        [ div [] [] ]
                     ]
                        ++ (List.map (thTag model) <| List.sortBy .name tags)
                    )
                ]
            , tbody [] (foldKMRows model)
            ]
        ]


thTag : Model -> Tag -> Html Msg
thTag model tag =
    let
        attributes =
            [ style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            , class "tag"
            ]
    in
    th [ class "th-tag", classList [ ( "highlighted", model.highlightedTagUuid == Just tag.uuid ) ] ]
        [ div []
            [ div attributes [ text tag.name ]
            ]
        ]


foldKMRows : Model -> List (Html Msg)
foldKMRows model =
    let
        tags =
            KnowledgeModel.getTags model.knowledgeModel

        chapters =
            KnowledgeModel.getChapters model.knowledgeModel
    in
    List.foldl (\c rows -> rows ++ foldChapter model tags c) [] chapters


foldChapter : Model -> List Tag -> Chapter -> List (Html Msg)
foldChapter model tags chapter =
    if List.length chapter.questionUuids > 0 then
        let
            questions =
                KnowledgeModel.getChapterQuestions chapter.uuid model.knowledgeModel
        in
        List.foldl (\q rows -> rows ++ foldQuestion model 1 tags q) [ trChapter chapter tags ] questions

    else
        []


foldQuestion : Model -> Int -> List Tag -> Question -> List (Html Msg)
foldQuestion model indent tags question =
    let
        questionRow =
            [ trQuestion model indent tags question ]
    in
    case question of
        OptionsQuestion commonData _ ->
            List.foldl
                (\a rows -> rows ++ foldAnswer model (indent + 1) tags a)
                questionRow
                (KnowledgeModel.getQuestionAnswers commonData.uuid model.knowledgeModel)

        ListQuestion commonData _ ->
            List.foldl
                (\q rows -> rows ++ foldQuestion model (indent + 2) tags q)
                (questionRow ++ [ trItemTemplate (indent + 1) tags ])
                (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid model.knowledgeModel)

        ValueQuestion _ _ ->
            questionRow

        IntegrationQuestion _ _ ->
            questionRow


foldAnswer : Model -> Int -> List Tag -> Answer -> List (Html Msg)
foldAnswer model indent tags answer =
    let
        followUps =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid model.knowledgeModel
    in
    if List.length followUps > 0 then
        List.foldl (\q rows -> rows ++ foldQuestion model (indent + 1) tags q) [ trAnswer answer indent tags ] followUps

    else
        []


trQuestion : Model -> Int -> List Tag -> Question -> Html Msg
trQuestion model indent tags question =
    tr []
        ([ th []
            [ div [ indentClass indent ]
                [ fa "comment-o"
                , text (Question.getTitle question)
                ]
            ]
         ]
            ++ (List.map (tdQuestionTagCheckbox model question) <| List.sortBy .name tags)
        )


tdQuestionTagCheckbox : Model -> Question -> Tag -> Html Msg
tdQuestionTagCheckbox model question tag =
    let
        hasTag =
            hasQuestionTag model (Question.getUuid question) tag.uuid

        msg =
            if hasTag then
                RemoveTag (Question.getUuid question) tag.uuid

            else
                AddTag (Question.getUuid question) tag.uuid
    in
    td
        [ class "td-checkbox"
        , classList [ ( "highlighted", model.highlightedTagUuid == Just tag.uuid ) ]
        , onMouseOver <| Highlight tag.uuid
        , onMouseOut <| CancelHighlight
        ]
        [ label [] [ input [ type_ "checkbox", checked hasTag, onClick msg ] [] ] ]


trChapter : Chapter -> List Tag -> Html Msg
trChapter chapter =
    trSeparator chapter.title "book" "separator-chapter" 0


trAnswer : Answer -> Int -> List Tag -> Html Msg
trAnswer answer =
    trSeparator answer.label "check-square-o" ""


trItemTemplate : Int -> List Tag -> Html Msg
trItemTemplate =
    trSeparator "Item Template" "file-text-o" ""


trSeparator : String -> String -> String -> Int -> List Tag -> Html Msg
trSeparator title icon extraClass indent tags =
    tr [ class <| "separator " ++ extraClass ]
        ([ th []
            [ div [ indentClass indent ]
                [ fa icon
                , text title
                ]
            ]
         ]
            ++ (List.map tdTag <| List.sortBy .name tags)
        )


tdTag : Tag -> Html Msg
tdTag tag =
    td
        [ onMouseOver <| Highlight tag.uuid
        , onMouseOut <| CancelHighlight
        ]
        []


indentClass : Int -> Attribute msg
indentClass indent =
    class <| (++) "indent-" <| String.fromInt indent
