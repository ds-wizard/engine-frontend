module KnowledgeModels.Editor.View exposing (..)

import Common.Html exposing (..)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (defaultFullPageError, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (Html, a, button, div, h3, i, input, label, li, option, select, text, textarea, ul)
import Html.Attributes exposing (class, href, rows, type_)
import Html.Events exposing (..)
import KnowledgeModels.Editor.Models exposing (..)
import KnowledgeModels.Editor.Models.Editors exposing (..)
import KnowledgeModels.Editor.Msgs exposing (..)
import List.Extra exposing (find)
import Msgs
import Reorderable
import Routing exposing (Route(..))
import String exposing (toLower)


view : Model -> Html Msgs.Msg
view model =
    div []
        [ pageHeader "Knowledge model editor" []
        , content model
        ]


content : Model -> Html Msgs.Msg
content model =
    case model.knowledgeModelEditor of
        Unset ->
            emptyNode

        Loading ->
            fullPageLoader

        Error err ->
            defaultFullPageError err

        Success knowledgeModel ->
            editorView model


editorView : Model -> Html Msgs.Msg
editorView model =
    let
        currentView =
            case model.knowledgeModelEditor of
                Success knowledgeModelEditor ->
                    viewKnowledgeModel model knowledgeModelEditor (Edit >> Msgs.KnowledgeModelsEditorMsg)

                _ ->
                    emptyNode
    in
    div []
        [ formResultView model.saving
        , div [ class "knowledge-model-editor col-xs-12 col-lg-10 col-lg-offset-1" ] [ currentView ]
        ]


viewKnowledgeModel : Model -> KnowledgeModelEditor -> (KnowledgeModelMsg -> Msgs.Msg) -> Html Msgs.Msg
viewKnowledgeModel model (KnowledgeModelEditor editor) parentMsg =
    let
        chapterActive (ChapterEditor editor) =
            editor.active

        activeChapter =
            find chapterActive editor.chapters

        content =
            case activeChapter of
                Just ((ChapterEditor editor) as chapterEditor) ->
                    [ viewChapter
                        model
                        chapterEditor
                        (ChapterMsg editor.chapter.uuid >> parentMsg)
                        (DeleteChapter editor.chapter.uuid |> parentMsg)
                    ]

                Nothing ->
                    let
                        formContent =
                            div []
                                [ inputGroup editor.form "name" "Name" ]
                                |> Html.map (KnowledgeModelFormMsg >> parentMsg)
                    in
                    [ editorTitle "Knowledge Model"
                    , formContent
                    , inputChildren
                        "Chapter"
                        model.reorderableState
                        editor.chapters
                        (ReorderChapterList >> parentMsg)
                        (AddChapter |> parentMsg)
                        (\(ChapterEditor chapterEditor) -> chapterEditor.chapter.uuid)
                        (\(ChapterEditor chapterEditor) -> (Form.getFieldAsString "title" chapterEditor.form).value |> Maybe.withDefault "")
                        (\(ChapterEditor chapterEditor) -> ViewChapter chapterEditor.chapter.uuid |> parentMsg)
                    , div [ class "form-actions" ]
                        [ linkTo KnowledgeModels
                            [ class "btn btn-default" ]
                            [ text "Cancel" ]
                        , actionButton ( "Save", model.saving, KnowledgeModelFormMsg Form.Submit |> parentMsg )
                        ]
                    ]
    in
    div [ class "knowledge-model" ]
        content


viewChapter : Model -> ChapterEditor -> (ChapterMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewChapter model (ChapterEditor editor) parentMsg deleteMsg =
    let
        questionActive (QuestionEditor qe) =
            qe.active

        activeQuestion =
            find questionActive editor.questions

        content =
            case activeQuestion of
                Just ((QuestionEditor qe) as questionEditor) ->
                    [ viewQuestion
                        model
                        questionEditor
                        (ChapterQuestionMsg qe.question.uuid >> parentMsg)
                        (DeleteChapterQuestion qe.question.uuid |> parentMsg)
                    ]

                Nothing ->
                    let
                        formContent =
                            div []
                                [ inputGroup editor.form "title" "Title"
                                , textAreaGroup editor.form "text" "Text"
                                ]
                                |> Html.map (ChapterFormMsg >> parentMsg)
                    in
                    [ editorTitle "Chapter"
                    , formContent
                    , inputChildren
                        "Question"
                        model.reorderableState
                        editor.questions
                        (ReorderQuestionList >> parentMsg)
                        (AddChapterQuestion |> parentMsg)
                        (\(QuestionEditor questionEditor) -> questionEditor.question.uuid)
                        (\(QuestionEditor questionEditor) -> (Form.getFieldAsString "title" questionEditor.form).value |> Maybe.withDefault "")
                        (\(QuestionEditor questionEditor) -> ViewQuestion questionEditor.question.uuid |> parentMsg)
                    , formActions (ChapterCancel |> parentMsg) deleteMsg (ChapterFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "chapter" ]
        content


viewQuestion : Model -> QuestionEditor -> (QuestionMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewQuestion model (QuestionEditor editor) parentMsg deleteMsg =
    let
        answerActive (AnswerEditor ae) =
            ae.active

        activeAnswer =
            find answerActive editor.answers

        content =
            case activeAnswer of
                Just ((AnswerEditor ae) as answerEditor) ->
                    [ viewAnswer
                        model
                        answerEditor
                        (AnswerMsg ae.answer.uuid >> parentMsg)
                        (DeleteAnswer ae.answer.uuid |> parentMsg)
                    ]

                Nothing ->
                    let
                        formContent =
                            div []
                                [ inputGroup editor.form "title" "Title"
                                , textAreaGroup editor.form "text" "Text"
                                ]
                                |> Html.map (QuestionFormMsg >> parentMsg)
                    in
                    [ editorTitle "Question"
                    , formContent
                    , inputChildren
                        "Answer"
                        model.reorderableState
                        editor.answers
                        (ReorderAnswerList >> parentMsg)
                        (AddAnswer |> parentMsg)
                        (\(AnswerEditor answerEditor) -> answerEditor.answer.uuid)
                        (\(AnswerEditor answerEditor) -> (Form.getFieldAsString "label" answerEditor.form).value |> Maybe.withDefault "")
                        (\(AnswerEditor answerEditor) -> ViewAnswer answerEditor.answer.uuid |> parentMsg)
                    , formActions (QuestionCancel |> parentMsg) deleteMsg (QuestionFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "question" ]
        content


viewAnswer : Model -> AnswerEditor -> (AnswerMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewAnswer model (AnswerEditor editor) parentMsg deleteMsg =
    let
        formContent =
            div []
                [ inputGroup editor.form "label" "Label"
                , textAreaGroup editor.form "advice" "Advice"
                ]
                |> Html.map (AnswerFormMsg >> parentMsg)
    in
    div [ class "answer" ]
        [ editorTitle "Answer"
        , formContent
        , formActions (AnswerCancel |> parentMsg) deleteMsg (AnswerFormMsg Form.Submit |> parentMsg)
        ]


inputChildren : String -> Reorderable.State -> List a -> (List a -> Msgs.Msg) -> Msgs.Msg -> (a -> String) -> (a -> String) -> (a -> Msgs.Msg) -> Html Msgs.Msg
inputChildren childName reorderableState children reorderMsg addMsg toId getName getMsg =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text (childName ++ "s") ]
        , Reorderable.ul
            (Reorderable.fullConfig
                { toId = toId
                , toMsg = ReorderableMsg >> Msgs.KnowledgeModelsEditorMsg
                , draggable = True
                , updateList = reorderMsg
                , itemView = inputChild getName getMsg
                , placeholderView = placeholderView
                , listClass = "input-children"
                , itemClass = "panel panel-default panel-input-child"
                , placeholderClass = "panel panel-placeholder panel-input-child"
                }
            )
            reorderableState
            children
        , a [ onClick addMsg, class "link-with-icon link-add-child" ]
            [ i [ class "fa fa-plus" ] []
            , text ("Add " ++ toLower childName)
            ]
        ]


inputChild : (a -> String) -> (a -> Msgs.Msg) -> Reorderable.HtmlWrapper Msgs.Msg -> a -> Html Msgs.Msg
inputChild getName getMsg ignoreDrag item =
    div []
        [ ignoreDrag a
            [ onClick <| getMsg item ]
            [ text <| getName item ]
        ]


placeholderView : a -> Html msg
placeholderView _ =
    div [] [ text "-" ]


editorTitle : String -> Html Msgs.Msg
editorTitle title =
    h3 [] [ text title ]


breadcrumbs_ : List String -> Html Msgs.Msg
breadcrumbs_ elements =
    ul [ class "breadcrumb" ]
        (List.map breadcrumbsElement_ elements)


breadcrumbsElement_ : String -> Html Msgs.Msg
breadcrumbsElement_ name =
    li [] [ text name ]


formActions : Msgs.Msg -> Msgs.Msg -> Msgs.Msg -> Html Msgs.Msg
formActions cancelMsg deleteMsg submitMsg =
    div [ class "form-actions" ]
        [ div []
            [ button [ class "btn btn-default", onClick cancelMsg ]
                [ text "Cancel" ]
            , button [ class "btn btn-link link-with-icon delete-entity", onClick deleteMsg ]
                [ i [ class "fa fa-trash-o" ] []
                , text "Delete"
                ]
            ]
        , button [ class "btn btn-primary", onClick submitMsg ]
            [ text "Save" ]
        ]
