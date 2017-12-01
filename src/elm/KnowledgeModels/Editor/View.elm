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
import KnowledgeModels.Editor.View.Breadcrumbs exposing (breadcrumbs, getKnowledgeModelBreadcrumbs)
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
        ( breadcrumbsView, currentView ) =
            case model.knowledgeModelEditor of
                Success knowledgeModelEditor ->
                    ( getKnowledgeModelBreadcrumbs knowledgeModelEditor |> breadcrumbs
                    , viewKnowledgeModel model knowledgeModelEditor (Edit >> Msgs.KnowledgeModelsEditorMsg)
                    )

                _ ->
                    ( emptyNode, emptyNode )
    in
    div []
        [ formResultView model.saving
        , breadcrumbsView
        , div [ class "knowledge-model-editor col-xs-12 col-lg-10 col-lg-offset-1" ] [ currentView ]
        ]


viewKnowledgeModel : Model -> KnowledgeModelEditor -> (KnowledgeModelMsg -> Msgs.Msg) -> Html Msgs.Msg
viewKnowledgeModel model (KnowledgeModelEditor editor) parentMsg =
    let
        content =
            case getActiveChapterEditor editor.chapters of
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
                        getChapterUuid
                        getChapterEditorName
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
        content =
            case getActiveQuestionEditor editor.questions of
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
                        getQuestionUuid
                        getQuestionEditorName
                        (\(QuestionEditor questionEditor) -> ViewQuestion questionEditor.question.uuid |> parentMsg)
                    , formActions (ChapterCancel |> parentMsg) deleteMsg (ChapterFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "chapter" ]
        content


viewQuestion : Model -> QuestionEditor -> (QuestionMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewQuestion model (QuestionEditor editor) parentMsg deleteMsg =
    let
        activeChild =
            ( getActiveAnswerEditor editor.answers
            , getActiveReferenceEditor editor.references
            , getActiveExpertEditor editor.experts
            )

        content =
            case activeChild of
                ( Just ((AnswerEditor ae) as answerEditor), _, _ ) ->
                    [ viewAnswer
                        model
                        answerEditor
                        (AnswerMsg ae.answer.uuid >> parentMsg)
                        (DeleteAnswer ae.answer.uuid |> parentMsg)
                    ]

                ( _, Just ((ReferenceEditor re) as referenceEditor), _ ) ->
                    [ viewReference
                        model
                        referenceEditor
                        (ReferenceMsg re.reference.uuid >> parentMsg)
                        (DeleteReference re.reference.uuid |> parentMsg)
                    ]

                ( _, _, Just ((ExpertEditor ee) as expertEditor) ) ->
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
                        getAnswerUuid
                        getAnswerEditorName
                        (\(AnswerEditor ae) -> ViewAnswer ae.answer.uuid |> parentMsg)
                    , inputChildren
                        "Reference"
                        model.reorderableState
                        editor.references
                        (ReorderReferenceList >> parentMsg)
                        (AddReference |> parentMsg)
                        getReferenceUuid
                        getReferenceEditorName
                        (\(ReferenceEditor re) -> ViewReference re.reference.uuid |> parentMsg)
                    , inputChildren
                        "Expert"
                        model.reorderableState
                        editor.experts
                        (ReorderExpertList >> parentMsg)
                        (AddExpert |> parentMsg)
                        getExpertUuid
                        getExpertEditorName
                        (\(ExpertEditor ee) -> ViewExpert ee.expert.uuid |> parentMsg)
                    , formActions (QuestionCancel |> parentMsg) deleteMsg (QuestionFormMsg Form.Submit |> parentMsg)
                    ]
    in
    div [ class "question" ]
        content


viewAnswer : Model -> AnswerEditor -> (AnswerMsg -> Msgs.Msg) -> Msgs.Msg -> Html Msgs.Msg
viewAnswer model (AnswerEditor editor) parentMsg deleteMsg =
    let
        content =
            case getActiveQuestionEditor editor.followUps of
                Just ((QuestionEditor qe) as questionEditor) ->
                    [ viewQuestion
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
