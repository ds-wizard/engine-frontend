module KMEditor.Editor.View.Editors exposing (activeEditor)

import ActionResult
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode, fa)
import Common.View exposing (fullPageMessage)
import Common.View.Forms exposing (formGroup, inputGroup, selectGroup, textAreaGroup, toggleGroup)
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Input as Input exposing (baseInput)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Entities exposing (Level, Metric)
import KMEditor.Editor.Models exposing (Model, getActiveEditor)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (AnswerForm, questionTypeOptions, referenceTypeOptions)
import KMEditor.Editor.Msgs exposing (..)
import Reorderable
import String exposing (toLower)


activeEditor : Model -> ( String, Html Msg )
activeEditor model =
    case getActiveEditor model of
        Just editor ->
            case editor of
                KMEditor data ->
                    kmEditorView model data

                ChapterEditor data ->
                    chapterEditorView model data

                QuestionEditor data ->
                    questionEditorView model data

                AnswerEditor data ->
                    answerEditorView model data

                ReferenceEditor data ->
                    referenceEditorView data

                ExpertEditor data ->
                    expertEditorView data

        Nothing ->
            ( "nothing", fullPageMessage "fa-long-arrow-left" "Select what you want to edit" )


getChildName : Dict String Editor -> String -> String
getChildName editors uuid =
    Dict.get uuid editors
        |> Maybe.map getEditorTitle
        |> Maybe.withDefault "-"


editorClass : String
editorClass =
    "col-xl-10 col-lg-12"


kmEditorView : Model -> KMEditorData -> ( String, Html Msg )
kmEditorView model editorData =
    let
        editorTitleConfig =
            { title = "Knowledge Model"
            , deleteAction = Nothing
            }

        chaptersConfig =
            { childName = "Chapter"
            , reorderableState = model.reorderableState
            , children = editorData.chapters.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderChapters >> KMEditorMsg >> EditorMsg
            , addMsg = AddChapter |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }

        form =
            div []
                [ inputGroup editorData.form "name" "Name"
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle editorTitleConfig
        , form |> Html.map (KMEditorFormMsg >> KMEditorMsg >> EditorMsg)
        , inputChildren chaptersConfig
        ]
    )


chapterEditorView : Model -> ChapterEditorData -> ( String, Html Msg )
chapterEditorView model editorData =
    let
        editorTitleConfig =
            { title = "Chapter"
            , deleteAction = DeleteChapter editorData.uuid |> ChapterEditorMsg |> EditorMsg |> Just
            }

        questionsConfig =
            { childName = "Question"
            , reorderableState = model.reorderableState
            , children = editorData.questions.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderQuestions >> ChapterEditorMsg >> EditorMsg
            , addMsg = AddQuestion |> ChapterEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }

        form =
            div []
                [ inputGroup editorData.form "title" "Title"
                , textAreaGroup editorData.form "text" "Text"
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle editorTitleConfig
        , form |> Html.map (ChapterFormMsg >> ChapterEditorMsg >> EditorMsg)
        , inputChildren questionsConfig
        ]
    )


questionEditorView : Model -> QuestionEditorData -> ( String, Html Msg )
questionEditorView model editorData =
    let
        editorTitleConfig =
            { title = "Question"
            , deleteAction = DeleteQuestion editorData.uuid |> QuestionEditorMsg |> EditorMsg |> Just
            }

        form =
            div []
                [ inputGroup editorData.form "title" "Title"
                , textAreaGroup editorData.form "text" "Text"
                , selectGroup questionTypeOptions editorData.form "type_" "Question Type"
                , p [ class "form-text text-muted" ]
                    [ fa "warning"
                    , text "By changing the type answers or items might be removed."
                    ]
                , questionRequiredLevelSelectGroup editorData <| ActionResult.withDefault [] <| model.levels
                ]

        answersOrItem =
            case (Form.getFieldAsString "type_" editorData.form).value of
                Just "options" ->
                    questionEditorAnswersView model editorData

                Just "list" ->
                    questionEditorAnswerItemTemplateView model editorData

                _ ->
                    emptyNode
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle editorTitleConfig
        , form |> Html.map (QuestionFormMsg >> QuestionEditorMsg >> EditorMsg)
        , answersOrItem
        , questionEditorReferencesView model editorData
        , questionEditorExpertsView model editorData
        ]
    )


questionRequiredLevelSelectGroup : QuestionEditorData -> List Level -> Html Form.Msg
questionRequiredLevelSelectGroup editorData levels =
    let
        options =
            levels
                |> List.map createLevelOption
                |> (::) ( "", "Never" )
    in
    selectGroup options editorData.form "requiredLevel" "When does this question become desirable?"


createLevelOption : Level -> ( String, String )
createLevelOption level =
    ( toString level.level, level.title )


questionEditorAnswersView : Model -> QuestionEditorData -> Html Msg
questionEditorAnswersView model editorData =
    inputChildren
        { childName = "Answer"
        , reorderableState = model.reorderableState
        , children = editorData.answers.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderAnswers >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddAnswer |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName model.editors
        , viewMsg = SetActiveEditor
        }


questionEditorAnswerItemTemplateView : Model -> QuestionEditorData -> Html Msg
questionEditorAnswerItemTemplateView model editorData =
    let
        config =
            { childName = "Item Question"
            , reorderableState = model.reorderableState
            , children = editorData.answerItemTemplateQuestions.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderAnswerItemTemplateQuestions >> QuestionEditorMsg >> EditorMsg
            , addMsg = AddAnswerItemTemplateQuestion |> QuestionEditorMsg >> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }
    in
    div [ class "card card-border-light mb-3" ]
        [ div [ class "card-header" ]
            [ text "Item template" ]
        , div [ class "card-body" ]
            [ div [ class "form-group" ]
                [ inputGroup editorData.form "itemName" "Item Title" |> Html.map (QuestionFormMsg >> QuestionEditorMsg >> EditorMsg)
                ]
            , inputChildren config
            ]
        ]


questionEditorReferencesView : Model -> QuestionEditorData -> Html Msg
questionEditorReferencesView model editorData =
    inputChildren
        { childName = "Reference"
        , reorderableState = model.reorderableState
        , children = editorData.references.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderReferences >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddReference |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName model.editors
        , viewMsg = SetActiveEditor
        }


questionEditorExpertsView : Model -> QuestionEditorData -> Html Msg
questionEditorExpertsView model editorData =
    inputChildren
        { childName = "Expert"
        , reorderableState = model.reorderableState
        , children = editorData.experts.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderExperts >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddExpert |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName model.editors
        , viewMsg = SetActiveEditor
        }


answerEditorView : Model -> AnswerEditorData -> ( String, Html Msg )
answerEditorView model editorData =
    let
        editorTitleConfig =
            { title = "Answer"
            , deleteAction = DeleteAnswer editorData.uuid |> AnswerEditorMsg |> EditorMsg |> Just
            }

        metrics =
            model.metrics
                |> ActionResult.map (metricsView editorData)
                |> ActionResult.withDefault emptyNode

        followUpsConfig =
            { childName = "Follow-up Question"
            , reorderableState = model.reorderableState
            , children = editorData.followUps.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderFollowUps >> AnswerEditorMsg >> EditorMsg
            , addMsg = AddFollowUp |> AnswerEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }

        form =
            div []
                [ inputGroup editorData.form "label" "Label"
                , textAreaGroup editorData.form "advice" "Advice"
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle editorTitleConfig
        , form |> Html.map (AnswerFormMsg >> AnswerEditorMsg >> EditorMsg)
        , metrics
        , inputChildren followUpsConfig
        ]
    )


metricsView : AnswerEditorData -> List Metric -> Html Msg
metricsView editorData metrics =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text "Metrics" ]
        , table [ class "table table-hover table-metrics" ]
            [ thead []
                [ tr []
                    [ th [] []
                    , th [] [ text "Weight" ]
                    , th [] [ text "Measure" ]
                    ]
                ]
            , tbody [] (List.indexedMap (metricView editorData.form) metrics)
            ]
        ]
        |> Html.map (AnswerFormMsg >> AnswerEditorMsg >> EditorMsg)


metricView : Form CustomFormError AnswerForm -> Int -> Metric -> Html Form.Msg
metricView form i metric =
    let
        enabled =
            Form.getFieldAsBool ("metricMeasures." ++ toString i ++ ".enabled") form
                |> .value
                |> Maybe.withDefault False
    in
    tr [ classList [ ( "disabled", not enabled ) ] ]
        [ td [] [ toggleGroup form ("metricMeasures." ++ toString i ++ ".enabled") metric.title ]
        , td [] [ metricInput form ("metricMeasures." ++ toString i ++ ".weight") enabled ]
        , td [] [ metricInput form ("metricMeasures." ++ toString i ++ ".measure") enabled ]
        ]


metricInput : Form CustomFormError o -> String -> Bool -> Html Form.Msg
metricInput form fieldName enabled =
    formGroup Input.textInput [ disabled (not enabled) ] form fieldName ""


referenceEditorView : ReferenceEditorData -> ( String, Html Msg )
referenceEditorView editorData =
    let
        editorTitleConfig =
            { title = "Reference"
            , deleteAction = DeleteReference editorData.uuid |> ReferenceEditorMsg |> EditorMsg |> Just
            }

        formFields =
            case (Form.getFieldAsString "referenceType" editorData.form).value of
                Just "ResourcePageReference" ->
                    [ inputGroup editorData.form "shortUuid" "Short UUID"
                    ]

                Just "URLReference" ->
                    [ inputGroup editorData.form "url" "URL"
                    , inputGroup editorData.form "label" "Label"
                    ]

                Just "CrossReference" ->
                    [ inputGroup editorData.form "targetUuid" "Target UUID"
                    , inputGroup editorData.form "description" "Description"
                    ]

                _ ->
                    []

        form =
            div []
                ([ selectGroup referenceTypeOptions editorData.form "referenceType" "Type" ]
                    ++ formFields
                )
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle editorTitleConfig
        , form |> Html.map (ReferenceFormMsg >> ReferenceEditorMsg >> EditorMsg)
        ]
    )


expertEditorView : ExpertEditorData -> ( String, Html Msg )
expertEditorView editorData =
    let
        editorTitleConfig =
            { title = "Expert"
            , deleteAction = DeleteExpert editorData.uuid |> ExpertEditorMsg |> EditorMsg |> Just
            }

        form =
            div []
                [ inputGroup editorData.form "name" "Name"
                , inputGroup editorData.form "email" "Email"
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle editorTitleConfig
        , form |> Html.map (ExpertFormMsg >> ExpertEditorMsg >> EditorMsg)
        ]
    )


type alias EditorTitleConfig =
    { title : String
    , deleteAction : Maybe Msg
    }


editorTitle : EditorTitleConfig -> Html Msg
editorTitle config =
    let
        deleteActionButton =
            case config.deleteAction of
                Just msg ->
                    button [ class "btn btn-outline-danger link-with-icon", onClick msg ]
                        [ fa "trash-o"
                        , text "Delete"
                        ]

                Nothing ->
                    emptyNode
    in
    div [ class "editor-title" ]
        [ h3 [] [ text config.title ]
        , deleteActionButton
        ]


type alias InputChildrenConfig a =
    { childName : String
    , reorderableState : Reorderable.State
    , children : List a
    , reorderMsg : List a -> Msg
    , addMsg : Msg
    , toId : a -> String
    , getName : a -> String
    , viewMsg : a -> Msg
    }


inputChildren : InputChildrenConfig a -> Html Msg
inputChildren config =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text (config.childName ++ "s") ]
        , Reorderable.ul
            (Reorderable.fullConfig
                { toId = config.toId
                , toMsg = ReorderableMsg
                , draggable = True
                , updateList = config.reorderMsg
                , itemView = inputChild config.getName config.viewMsg
                , placeholderView = placeholderView
                , listClass = "input-children"
                , itemClass = "input-child"
                , placeholderClass = "input-child input-child-placeholder"
                }
            )
            config.reorderableState
            config.children
        , a [ onClick config.addMsg, class "link-with-icon link-add-child" ]
            [ i [ class "fa fa-plus" ] []
            , text ("Add " ++ toLower config.childName)
            ]
        ]


inputChild : (a -> String) -> (a -> Msg) -> Reorderable.HtmlWrapper Msg -> a -> Html Msg
inputChild getName viewMsg ignoreDrag item =
    div []
        [ ignoreDrag a
            [ onClick <| viewMsg item ]
            [ text <| getName item ]
        ]


placeholderView : a -> Html msg
placeholderView _ =
    div [] [ text "-" ]
