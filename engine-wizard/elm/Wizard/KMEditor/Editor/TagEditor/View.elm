module Wizard.KMEditor.Editor.TagEditor.View exposing (view)

import Html exposing (Attribute, Html, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, classList, style, type_)
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l)
import Shared.Utils exposing (getContrastColorHex)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash
import Wizard.KMEditor.Editor.TagEditor.Models exposing (Model, hasQuestionTag)
import Wizard.KMEditor.Editor.TagEditor.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.TagEditor.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            if List.length model.knowledgeModel.tagUuids > 0 then
                if (List.length <| KnowledgeModel.getAllQuestions model.knowledgeModel) > 0 then
                    tagEditorTable appState model

                else
                    Flash.info appState <| l_ "noQuestions" appState

            else
                Flash.info appState <| l_ "noTags" appState
    in
    div [ class "KMEditor__Editor__TagEditor", dataCy "km-editor_tags" ]
        [ content ]


tagEditorTable : AppState -> Model -> Html Msg
tagEditorTable appState model =
    let
        tags =
            KnowledgeModel.getTags model.knowledgeModel
    in
    div [ class "editor-table-container" ]
        [ table []
            [ thead []
                [ tr []
                    (th [ class "top-left" ] [ div [] [] ]
                        :: (List.map (thTag model) <| List.sortBy .name tags)
                    )
                ]
            , tbody [] (foldKMRows appState model)
            ]
        ]


thTag : Model -> Tag -> Html Msg
thTag model tag =
    let
        attributes =
            [ style "background" tag.color
            , style "color" <| getContrastColorHex tag.color
            , class "tag"
            , dataCy "km-editor_tag-editor_tag"
            ]
    in
    th [ class "th-tag", classList [ ( "highlighted", model.highlightedTagUuid == Just tag.uuid ) ] ]
        [ div []
            [ div attributes [ text tag.name ]
            ]
        ]


foldKMRows : AppState -> Model -> List (Html Msg)
foldKMRows appState model =
    let
        tags =
            KnowledgeModel.getTags model.knowledgeModel

        chapters =
            KnowledgeModel.getChapters model.knowledgeModel
    in
    List.foldl (\c rows -> rows ++ foldChapter appState model tags c) [] chapters


foldChapter : AppState -> Model -> List Tag -> Chapter -> List (Html Msg)
foldChapter appState model tags chapter =
    if List.length chapter.questionUuids > 0 then
        let
            questions =
                KnowledgeModel.getChapterQuestions chapter.uuid model.knowledgeModel
        in
        List.foldl (\q rows -> rows ++ foldQuestion appState model 1 tags q) [ trChapter appState chapter tags ] questions

    else
        []


foldQuestion : AppState -> Model -> Int -> List Tag -> Question -> List (Html Msg)
foldQuestion appState model indent tags question =
    let
        questionRow =
            [ trQuestion appState model indent tags question ]
    in
    case question of
        OptionsQuestion commonData _ ->
            List.foldl
                (\a rows -> rows ++ foldAnswer appState model (indent + 1) tags a)
                questionRow
                (KnowledgeModel.getQuestionAnswers commonData.uuid model.knowledgeModel)

        ListQuestion commonData _ ->
            List.foldl
                (\q rows -> rows ++ foldQuestion appState model (indent + 2) tags q)
                (questionRow ++ [ trItemTemplate appState (indent + 1) tags ])
                (KnowledgeModel.getQuestionItemTemplateQuestions commonData.uuid model.knowledgeModel)

        ValueQuestion _ _ ->
            questionRow

        IntegrationQuestion _ _ ->
            questionRow

        MultiChoiceQuestion _ _ ->
            questionRow


foldAnswer : AppState -> Model -> Int -> List Tag -> Answer -> List (Html Msg)
foldAnswer appState model indent tags answer =
    let
        followUps =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid model.knowledgeModel
    in
    if List.length followUps > 0 then
        List.foldl (\q rows -> rows ++ foldQuestion appState model (indent + 1) tags q) [ trAnswer appState answer indent tags ] followUps

    else
        []


trQuestion : AppState -> Model -> Int -> List Tag -> Question -> Html Msg
trQuestion appState model indent tags question =
    tr []
        (th [ onClick <| CopyUuid <| Question.getUuid question ]
            [ div [ indentClass indent ]
                [ faSet "km.question" appState
                , text (Question.getTitle question)
                ]
            ]
            :: (List.map (tdQuestionTagCheckbox model question) <| List.sortBy .name tags)
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
        [ label []
            [ input
                [ type_ "checkbox"
                , checked hasTag
                , onClick msg
                , dataCy ("km-editor_tag-editor_row_question-" ++ Question.getUuid question ++ "_" ++ "tag-" ++ tag.uuid)
                ]
                []
            ]
        ]


trChapter : AppState -> Chapter -> List Tag -> Html Msg
trChapter appState chapter =
    trSeparator (Just chapter.uuid) chapter.title (faSet "km.chapter" appState) "separator-chapter" 0


trAnswer : AppState -> Answer -> Int -> List Tag -> Html Msg
trAnswer appState answer =
    trSeparator (Just answer.uuid) answer.label (faSet "km.answer" appState) ""


trItemTemplate : AppState -> Int -> List Tag -> Html Msg
trItemTemplate appState =
    trSeparator Nothing "Item Template" (faSet "km.itemTemplate" appState) ""


trSeparator : Maybe String -> String -> Html Msg -> String -> Int -> List Tag -> Html Msg
trSeparator mbUuid title icon extraClass indent tags =
    let
        thAttributes =
            case mbUuid of
                Just uuid ->
                    [ onClick <| CopyUuid uuid ]

                Nothing ->
                    []
    in
    tr [ class <| "separator " ++ extraClass ]
        (th thAttributes
            [ div [ indentClass indent ]
                [ icon
                , text title
                ]
            ]
            :: (List.map tdTag <| List.sortBy .name tags)
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
