module KMEditor.Editor.View exposing (view)

import Common.Html exposing (..)
import Common.View exposing (defaultFullPageError, fullPageActionResultView, fullPageLoader, modalView, pageHeader)
import Common.View.Forms exposing (..)
import Form exposing (Form)
import Html exposing (..)
import Html.Attributes exposing (class, href, rows, type_)
import Html.Events exposing (..)
import KMEditor.Common.View exposing (diffTreeView)
import KMEditor.Editor.Models exposing (..)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (questionTypeOptions)
import KMEditor.Editor.Msgs exposing (..)
import KMEditor.Editor.View.Breadcrumbs exposing (breadcrumbs)
import KMEditor.Routing exposing (Route(Index))
import Msgs
import Reorderable
import Routing exposing (Route(..))
import String exposing (toLower)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "row KMEditor__Editor" ]
        [ div [ class "col-xs-12" ] [ pageHeader "Knowledge model editor" [] ]
        , fullPageActionResultView (editorView wrapMsg model) model.knowledgeModelEditor
        ]


editorView : (Msg -> Msgs.Msg) -> Model -> KnowledgeModelEditor -> Html Msgs.Msg
editorView wrapMsg model knowledgeModelEditor =
    div []
        [ div [ class "col-xs-12" ]
            [ formResultView model.saving ]
        , div [ class "col-xs-8" ]
            [ breadcrumbs knowledgeModelEditor
            , viewKnowledgeModel wrapMsg model knowledgeModelEditor (wrapMsg << Edit)
            ]
        , div [ class "col-xs-4 diff-tree-col" ]
            [ h4 [] [ text "Current changes" ]
            , diffTreeView (getKnowledgeModel knowledgeModelEditor) model.events
            ]
        ]


viewKnowledgeModel : (Msg -> Msgs.Msg) -> Model -> KnowledgeModelEditor -> (KnowledgeModelMsg -> Msgs.Msg) -> Html Msgs.Msg
viewKnowledgeModel wrapMsg model (KnowledgeModelEditor editor) parentMsg =
    let
        content =
            case getActiveChapterEditor editor.chapters of
                Just ((ChapterEditor editor) as chapterEditor) ->
                    [ viewChapter
                        wrapMsg
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
                        wrapMsg
                        "Chapter"
                        model.reorderableState
                        editor.chapters
                        (ReorderChapterList >> parentMsg)
                        (AddChapter |> parentMsg)
                        getChapterUuid
                        getChapterEditorName
                        (\(ChapterEditor chapterEditor) -> ViewChapter chapterEditor.chapter.uuid |> parentMsg)
                    , div [ class "form-actions" ]
                        [ linkTo (KMEditor Index)
                            [ class "btn btn-default" ]
                            [ text "Cancel" ]
                        , actionButton ( "Save", model.saving, KnowledgeModelFormMsg Form.Submit |> parentMsg )
                        ]
                    ]
    in
    div [ class "knowledge-model" ]
        content


viewChapter : (Msg -> Msgs.Msg) -> Model -> ChapterEditor -> (ChapterMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewChapter wrapMsg model (ChapterEditor editor) parentMsg deleteMsg =
    let
        content =
            case getActiveQuestionEditor editor.questions of
                Just ((QuestionEditor qe) as questionEditor) ->
                    [ viewQuestion
                        wrapMsg
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
                        wrapMsg
                        "Question"
                        model.reorderableState
                        editor.questions
                        (ReorderQuestionList >> parentMsg)
                        (AddChapterQuestion |> parentMsg)
                        getQuestionUuid
                        getQuestionEditorName
                        (\(QuestionEditor questionEditor) -> ViewQuestion questionEditor.question.uuid |> parentMsg)
                    , formActions (ChapterCancel |> parentMsg) deleteMsg (ChapterFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "chapter" ]
        content


viewQuestion : (Msg -> Msgs.Msg) -> Model -> QuestionEditor -> (QuestionMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewQuestion wrapMsg model (QuestionEditor editor) parentMsg deleteMsg =
    let
        activeChild =
            ( getActiveQuestionEditor editor.answerItemTemplateQuestions
            , getActiveAnswerEditor editor.answers
            , getActiveReferenceEditor editor.references
            , getActiveExpertEditor editor.experts
            )

        content =
            case activeChild of
                ( Just ((QuestionEditor qe) as questionEditor), _, _, _ ) ->
                    [ viewQuestion
                        wrapMsg
                        model
                        questionEditor
                        (AnswerItemTemplateQuestionMsg qe.question.uuid >> parentMsg)
                        (DeleteAnswerItemTemplateQuestion qe.question.uuid |> parentMsg)
                    ]

                ( _, Just ((AnswerEditor ae) as answerEditor), _, _ ) ->
                    [ viewAnswer
                        wrapMsg
                        model
                        answerEditor
                        (AnswerMsg ae.answer.uuid >> parentMsg)
                        (DeleteAnswer ae.answer.uuid |> parentMsg)
                    ]

                ( _, _, Just ((ReferenceEditor re) as referenceEditor), _ ) ->
                    [ viewReference
                        model
                        referenceEditor
                        (ReferenceMsg re.reference.uuid >> parentMsg)
                        (DeleteReference re.reference.uuid |> parentMsg)
                    ]

                ( _, _, _, Just ((ExpertEditor ee) as expertEditor) ) ->
                    [ viewExpert
                        model
                        expertEditor
                        (ExpertMsg ee.expert.uuid >> parentMsg)
                        (DeleteExpert ee.expert.uuid |> parentMsg)
                    ]

                _ ->
                    let
                        formContent =
                            div []
                                [ inputGroup editor.form "title" "Title"
                                , inputGroup editor.form "shortUuid" "Short UUID"
                                , textAreaGroup editor.form "text" "Text"
                                , selectGroup questionTypeOptions editor.form "type_" "Question Type"
                                ]
                                |> Html.map (QuestionFormMsg >> parentMsg)

                        answers =
                            case (Form.getFieldAsString "type_" editor.form).value of
                                Just "options" ->
                                    inputChildren
                                        wrapMsg
                                        "Answer"
                                        model.reorderableState
                                        editor.answers
                                        (ReorderAnswerList >> parentMsg)
                                        (AddAnswer |> parentMsg)
                                        getAnswerUuid
                                        getAnswerEditorName
                                        (\(AnswerEditor ae) -> ViewAnswer ae.answer.uuid |> parentMsg)

                                _ ->
                                    emptyNode

                        answerItemTemplate =
                            case (Form.getFieldAsString "type_" editor.form).value of
                                Just "list" ->
                                    div [ class "panel panel-default" ]
                                        [ div [ class "panel-heading" ]
                                            [ text "Item template" ]
                                        , div [ class "panel-body" ]
                                            [ div [ class "form-group" ]
                                                [ inputGroup editor.form "itemName" "Item Title" ]
                                                |> Html.map (QuestionFormMsg >> parentMsg)
                                            , inputChildren
                                                wrapMsg
                                                "Item Question"
                                                model.reorderableState
                                                editor.answerItemTemplateQuestions
                                                (ReorderAnswerItemTemplateQuestions >> parentMsg)
                                                (AddAnswerItemTemplateQuestion |> parentMsg)
                                                getQuestionUuid
                                                getQuestionEditorName
                                                (\(QuestionEditor qe) -> ViewAnswerItemTemplateQuestion qe.question.uuid |> parentMsg)
                                            ]
                                        ]

                                _ ->
                                    emptyNode

                        references =
                            inputChildren
                                wrapMsg
                                "Reference"
                                model.reorderableState
                                editor.references
                                (ReorderReferenceList >> parentMsg)
                                (AddReference |> parentMsg)
                                getReferenceUuid
                                getReferenceEditorName
                                (\(ReferenceEditor re) -> ViewReference re.reference.uuid |> parentMsg)

                        experts =
                            inputChildren
                                wrapMsg
                                "Expert"
                                model.reorderableState
                                editor.experts
                                (ReorderExpertList >> parentMsg)
                                (AddExpert |> parentMsg)
                                getExpertUuid
                                getExpertEditorName
                                (\(ExpertEditor ee) -> ViewExpert ee.expert.uuid |> parentMsg)
                    in
                    [ editorTitle "Question"
                    , formContent
                    , answerItemTemplate
                    , answers
                    , references
                    , experts
                    , formActions (QuestionCancel |> parentMsg) deleteMsg (QuestionFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "question" ]
        content


viewAnswer : (Msg -> Msgs.Msg) -> Model -> AnswerEditor -> (AnswerMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewAnswer wrapMsg model (AnswerEditor editor) parentMsg deleteMsg =
    let
        content =
            case getActiveQuestionEditor editor.followUps of
                Just ((QuestionEditor qe) as questionEditor) ->
                    [ viewQuestion
                        wrapMsg
                        model
                        questionEditor
                        (FollowUpQuestionMsg qe.question.uuid >> parentMsg)
                        (DeleteFollowUpQuestion qe.question.uuid |> parentMsg)
                    ]

                Nothing ->
                    let
                        formContent =
                            div []
                                [ inputGroup editor.form "label" "Label"
                                , textAreaGroup editor.form "advice" "Advice"
                                ]
                                |> Html.map (AnswerFormMsg >> parentMsg)
                    in
                    [ editorTitle "Answer"
                    , formContent
                    , inputChildren
                        wrapMsg
                        "Follow-up Question"
                        model.reorderableState
                        editor.followUps
                        (ReorderFollowUpQuestionList >> parentMsg)
                        (AddFollowUpQuestion |> parentMsg)
                        getQuestionUuid
                        getQuestionEditorName
                        (\(QuestionEditor questionEditor) -> ViewFollowUpQuestion questionEditor.question.uuid |> parentMsg)
                    , formActions (AnswerCancel |> parentMsg) deleteMsg (AnswerFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "answer" ]
        content


viewReference : Model -> ReferenceEditor -> (ReferenceMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewReference model (ReferenceEditor editor) parentMsg deleteMsg =
    let
        formContent =
            div []
                [ inputGroup editor.form "chapter" "Chapter"
                ]
                |> Html.map (ReferenceFormMsg >> parentMsg)
    in
    div [ class "reference" ]
        [ editorTitle "Reference"
        , formContent
        , formActions (ReferenceCancel |> parentMsg) deleteMsg (ReferenceFormMsg Form.Submit |> parentMsg)
        ]


viewExpert : Model -> ExpertEditor -> (ExpertMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewExpert model (ExpertEditor editor) parentMsg deleteMsg =
    let
        formContent =
            div []
                [ inputGroup editor.form "name" "Name"
                , inputGroup editor.form "email" "Email"
                ]
                |> Html.map (ExpertFormMsg >> parentMsg)
    in
    div [ class "expert" ]
        [ editorTitle "Expert"
        , formContent
        , formActions (ExpertCancel |> parentMsg) deleteMsg (ExpertFormMsg Form.Submit |> parentMsg)
        ]


inputChildren : (Msg -> Msgs.Msg) -> String -> Reorderable.State -> List a -> (List a -> Msgs.Msg) -> Msgs.Msg -> (a -> String) -> (a -> String) -> (a -> Msgs.Msg) -> Html Msgs.Msg
inputChildren wrapMsg childName reorderableState children reorderMsg addMsg toId getName getMsg =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text (childName ++ "s") ]
        , Reorderable.ul
            (Reorderable.fullConfig
                { toId = toId
                , toMsg = wrapMsg << ReorderableMsg
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


formActions : Msgs.Msg -> Msgs.Msg -> Msgs.Msg -> Html Msgs.Msg
formActions cancelMsg deleteMsg submitMsg =
    div [ class "form-actions" ]
        [ div []
            [ button [ class "btn btn-default", onClick cancelMsg ]
                [ text "Cancel" ]
            , button [ class "btn btn-link link-with-icon extra-action", onClick deleteMsg ]
                [ i [ class "fa fa-trash-o" ] []
                , text "Delete"
                ]
            ]
        , button [ class "btn btn-primary", onClick submitMsg ]
            [ text "Done" ]
        ]
