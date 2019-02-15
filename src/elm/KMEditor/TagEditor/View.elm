module KMEditor.TagEditor.View exposing (view)

import Common.Html exposing (emptyNode, fa)
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.TagEditor.Models exposing (Model, containsChanges, hasQuestionTag)
import KMEditor.TagEditor.Msgs exposing (Msg(..))
import Msgs
import Utils exposing (getContrastColorHex)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "col KMEditor__TagEditor" ]
        [ Page.actionResultView (editorView wrapMsg model) model.knowledgeModel ]


editorView : (Msg -> Msgs.Msg) -> Model -> KnowledgeModel -> Html Msgs.Msg
editorView wrapMsg model knowledgeModel =
    let
        unsavedChanges =
            if containsChanges model then
                div []
                    [ text "(unsaved changes)"
                    , button [ onClick Discard, class "btn btn-secondary btn-with-loader" ] [ text "Discard" ]
                    , ActionButton.button ( "Save", model.submitting, Submit )
                    ]

            else
                emptyNode
    in
    div [ class "row" ]
        [ div [ class "editor-header" ]
            [ text "Knowledge Model Tag Editor"
            , FormResult.errorOnlyView model.submitting
            , unsavedChanges
            ]
        , div [ class "editor-table-container" ]
            [ table []
                [ thead []
                    [ tr []
                        ([ th [ class "top-left" ]
                            [ div [] [] ]
                         ]
                            ++ (List.map (thTag model) <| List.sortBy .name knowledgeModel.tags)
                        )
                    ]
                , tbody [] (foldKMRows model knowledgeModel)
                ]
            ]
        ]
        |> Html.map wrapMsg


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


foldKMRows : Model -> KnowledgeModel -> List (Html Msg)
foldKMRows model km =
    List.foldl (\c rows -> rows ++ foldChapter model km.tags c) [] km.chapters


foldChapter : Model -> List Tag -> Chapter -> List (Html Msg)
foldChapter model tags chapter =
    if List.length chapter.questions > 0 then
        List.foldl (\q rows -> rows ++ foldQuestion model 1 tags q) [ trChapter chapter tags ] chapter.questions

    else
        []


foldQuestion : Model -> Int -> List Tag -> Question -> List (Html Msg)
foldQuestion model indent tags question =
    let
        questionRow =
            [ trQuestion model indent tags question ]
    in
    case question of
        OptionsQuestion questionData ->
            List.foldl
                (\a rows -> rows ++ foldAnswer model (indent + 1) tags a)
                questionRow
                questionData.answers

        ListQuestion questionData ->
            List.foldl
                (\q rows -> rows ++ foldQuestion model (indent + 2) tags q)
                (questionRow ++ [ trItemTemplate (indent + 1) tags ])
                questionData.itemQuestions

        ValueQuestion _ ->
            questionRow


foldAnswer : Model -> Int -> List Tag -> Answer -> List (Html Msg)
foldAnswer model indent tags answer =
    let
        followUps =
            getFollowUpQuestions answer
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
                , text (getQuestionTitle question)
                ]
            ]
         ]
            ++ (List.map (tdQuestionTagCheckbox model question) <| List.sortBy .name tags)
        )


tdQuestionTagCheckbox : Model -> Question -> Tag -> Html Msg
tdQuestionTagCheckbox model question tag =
    let
        hasTag =
            hasQuestionTag model (getQuestionUuid question) tag.uuid

        msg =
            if hasTag then
                RemoveTag (getQuestionUuid question) tag.uuid

            else
                AddTag (getQuestionUuid question) tag.uuid
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
