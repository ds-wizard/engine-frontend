module Common.Pages.KMEditor.Editor.Matrix exposing (Column, Config, view)

{-| Every question in the knowledge model down the side, something across the
top, and a checkbox in each cell: the Question Tags tab and the Phases tab are
the same page with a different column.

**It stays a real table**, with header cells in both directions and each cell's
checkbox named by both of them ("Data description, Core"). A grid of unlabeled
checkboxes is the thing a screen reader cannot read at all, and it is what the
wizard's markup is: its inputs carry no name, and the row's name is in a `td`
rather than a `th`.

The questions are walked the way the tree walks them — a chapter, its
questions, an options question's answers and their follow-ups, a list
question's item questions — with a separator row naming each parent, so the
column of checkboxes reads as one list rather than a tree flattened by accident.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, div, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, checked, class, classList, id, type_)
import Html.Events exposing (onClick)
import List.Extra as List
import Ui.Alert as Alert
import Ui.Icon as Icon exposing (Icon)
import Ui.Link as Link
import Ui.Tag as Tag
import Ui.TestId as TestId


type alias Column =
    { uuid : String
    , label : String
    , color : Maybe String
    }


type alias Config msg =
    { locale : Gettext.Locale
    , id : String

    -- Each checkbox's test hook is this, the question's uuid and the column's
    -- uuid, joined by underscores.
    , testId : String
    , label : String
    , columns : List Column

    -- No columns at all, and no questions at all, are the two empty states.
    , noColumns : String
    , isChecked : Question -> String -> Bool
    , onToggle : Question -> String -> msg

    -- A question's name is the way back to it: the matrix says which questions
    -- carry a tag, and the next thing anybody wants is the question itself.
    , questionUrl : String -> String
    }


view : Config msg -> EditorContext -> Html msg
view config editorContext =
    if List.isEmpty config.columns then
        Alert.toHtml (Alert.info config.noColumns)

    else if Dict.isEmpty editorContext.kmEditor.knowledgeModel.entities.questions then
        Alert.toHtml (Alert.info (gettext "There are no questions, create them first." config.locale))

    else
        div [ class "c-km-editor-matrix" ]
            [ table [ attribute "aria-label" config.label ]
                [ thead []
                    [ tr []
                        (th [ class "c-km-editor-matrix__corner", attribute "scope" "col" ] [ text (gettext "Question" config.locale) ]
                            :: List.map (viewColumnHeader config) config.columns
                        )
                    ]
                , tbody [] (rows config editorContext)
                ]
            ]


viewColumnHeader : Config msg -> Column -> Html msg
viewColumnHeader config column =
    let
        withColor =
            case column.color of
                Just color ->
                    Tag.withColor color

                Nothing ->
                    identity
    in
    th
        [ class "c-km-editor-matrix__column"
        , attribute "scope" "col"
        , id (columnId config column.uuid)
        ]
        [ Tag.toHtml (withColor (Tag.tag (named config column.label))) ]


columnId : Config msg -> String -> String
columnId config uuid =
    config.id ++ "-column-" ++ uuid


rowId : Config msg -> String -> String
rowId config uuid =
    config.id ++ "-row-" ++ uuid



-- THE ROWS


rows : Config msg -> EditorContext -> List (Html msg)
rows config editorContext =
    KnowledgeModel.getChapters editorContext.kmEditor.knowledgeModel
        |> kept .uuid editorContext
        |> List.concatMap (chapterRows config editorContext)


chapterRows : Config msg -> EditorContext -> Chapter -> List (Html msg)
chapterRows config editorContext chapter =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid editorContext.kmEditor.knowledgeModel
                |> kept Question.getUuid editorContext
    in
    if List.isEmpty questions then
        []

    else
        separator config True 0 (Icon.regular "fa-file") (named config chapter.title)
            :: List.concatMap (questionRows config editorContext 1) questions


questionRows : Config msg -> EditorContext -> Int -> Question -> List (Html msg)
questionRows config editorContext level question =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        uuid =
            Question.getUuid question

        children =
            case question of
                OptionsQuestion _ _ ->
                    KnowledgeModel.getQuestionAnswers uuid km
                        |> kept .uuid editorContext
                        |> List.concatMap
                            (\answer ->
                                let
                                    followUps =
                                        KnowledgeModel.getAnswerFollowupQuestions answer.uuid km
                                            |> kept Question.getUuid editorContext
                                in
                                if List.isEmpty followUps then
                                    []

                                else
                                    separator config False (level + 1) (Icon.regular "fa-circle-dot") (named config answer.label)
                                        :: List.concatMap (questionRows config editorContext (level + 2)) followUps
                            )

                ListQuestion _ _ ->
                    let
                        itemQuestions =
                            KnowledgeModel.getQuestionItemTemplateQuestions uuid km
                                |> kept Question.getUuid editorContext
                    in
                    if List.isEmpty itemQuestions then
                        []

                    else
                        separator config False (level + 1) (Icon.solid "fa-list") (gettext "Item" config.locale)
                            :: List.concatMap (questionRows config editorContext (level + 2)) itemQuestions

                _ ->
                    []
    in
    questionRow config question level :: children


questionRow : Config msg -> Question -> Int -> Html msg
questionRow config question level =
    let
        uuid =
            Question.getUuid question
    in
    tr [ class "c-km-editor-matrix__question" ]
        (th
            [ attribute "scope" "row"
            , id (rowId config uuid)
            , class "c-km-editor-matrix__name"
            , indent level
            ]
            [ div [ class "c-km-editor-matrix__label" ]
                [ Icon.toHtml (Icon.regular "fa-comment")
                , Link.toHtml
                    (Link.link
                        { url = config.questionUrl uuid
                        , label = named config (Question.getTitle question)
                        }
                    )
                ]
            ]
            :: List.map (cell config question) config.columns
        )


{-| The cell's checkbox is named by its row and its column, so it is "Data
description, Core" rather than an unnamed box in a grid.
-}
cell : Config msg -> Question -> Column -> Html msg
cell config question column =
    td [ class "c-km-editor-matrix__cell" ]
        [ input
            [ type_ "checkbox"
            , checked (config.isChecked question column.uuid)
            , onClick (config.onToggle question column.uuid)
            , attribute "aria-labelledby"
                (rowId config (Question.getUuid question) ++ " " ++ columnId config column.uuid)
            , TestId.attribute (config.testId ++ "_" ++ Question.getUuid question ++ "_" ++ column.uuid)
            ]
            []
        ]


{-| A row that names a parent rather than holding checkboxes: a chapter, an
answer, an item. It is built as a question's row is — a name in the column that
stays and an empty cell under each of the others — so the name keeps its place
while the columns move.
-}
separator : Config msg -> Bool -> Int -> Icon -> String -> Html msg
separator config isChapter level icon label =
    tr [ class "c-km-editor-matrix__separator" ]
        (th
            [ attribute "scope" "row"
            , classList
                [ ( "c-km-editor-matrix__name", True )
                , ( "is-chapter", isChapter )
                ]
            , indent level
            ]
            [ div [ class "c-km-editor-matrix__label" ]
                [ Icon.toHtml icon, text label ]
            ]
            -- The rest of the row is empty cells rather than one that spans
            -- them: the name has to stay put while the columns move, as a
            -- question's does, and the rule down the side of the column that
            -- stays has to carry on through.
            :: List.map (\_ -> td [ class "c-km-editor-matrix__cell" ] []) config.columns
        )


indent : Int -> Html.Attribute msg
indent level =
    attribute "style" ("--c-km-editor-matrix-level:" ++ String.fromInt level)


named : Config msg -> String -> String
named config value =
    if String.isEmpty value then
        gettext "Untitled" config.locale

    else
        value


kept : (a -> String) -> EditorContext -> List a -> List a
kept toUuid editorContext =
    List.filterNot (\entity -> EditorContext.isDeleted (toUuid entity) editorContext)
