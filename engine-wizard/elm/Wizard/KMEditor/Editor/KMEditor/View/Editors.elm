module Wizard.KMEditor.Editor.KMEditor.View.Editors exposing (activeEditor)

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, title)
import Html.Events exposing (onClick)
import List.Extra as List
import Reorderable
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lf, lg, lgx, lx)
import Shared.Utils exposing (httpMethodOptions)
import String exposing (fromInt, toLower)
import ValueList
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model, getActiveEditor, getCurrentIntegrations, getCurrentMetrics, getCurrentPhases, getCurrentTags)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (AnswerForm, IntegrationForm, QuestionForm, questionTypeOptions, questionValueTypeOptions, referenceTypeOptions)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (..)


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.View.Editors"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.KMEditor.Editor.KMEditor.View.Editors"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.KMEditor.View.Editors"


activeEditor : AppState -> String -> Model -> ( String, Html Msg )
activeEditor appState kmName model =
    case getActiveEditor model of
        Just editor ->
            case editor of
                KMEditor data ->
                    kmEditorView appState kmName model data

                MetricEditor data ->
                    metricEditorView appState model data

                PhaseEditor data ->
                    phaseEditorView appState model data

                TagEditor data ->
                    tagEditorView appState model data

                IntegrationEditor data ->
                    integrationEditorView appState model data

                ChapterEditor data ->
                    chapterEditorView appState kmName model data

                QuestionEditor data ->
                    questionEditorView appState kmName model data

                AnswerEditor data ->
                    answerEditorView appState kmName model data

                ChoiceEditor data ->
                    choiceEditorView appState data

                ReferenceEditor data ->
                    referenceEditorView appState data

                ExpertEditor data ->
                    expertEditorView appState data

        Nothing ->
            ( "nothing"
            , Page.message
                (faSet "_global.arrowLeft" appState)
                "km-editor-empty"
                (l_ "activeEditor.nothing" appState)
            )


getChildName : String -> Dict String Editor -> String -> String
getChildName kmName editors uuid =
    Dict.get uuid editors
        |> Maybe.map (getEditorTitle kmName)
        |> Maybe.withDefault "-"


editorClass : String
editorClass =
    "col-xl-10 col-lg-12"


kmEditorView : AppState -> String -> Model -> KMEditorData -> ( String, Html Msg )
kmEditorView appState kmName model editorData =
    let
        editorTitleConfig =
            { title = lg "knowledgeModel" appState
            , uuid = editorData.uuid
            , deleteAction = Nothing
            , movable = False
            }

        chaptersConfig =
            { childName = lg "chapter" appState
            , childNamePlural = lg "chapters" appState
            , reorderableState = model.reorderableState
            , children = editorData.chapters.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderChapters >> KMEditorMsg >> EditorMsg
            , addMsg = AddChapter |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "chapter"
            }

        metricsConfig =
            { childName = lg "metric" appState
            , childNamePlural = lg "metrics" appState
            , reorderableState = model.reorderableState
            , children = editorData.metrics.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderMetrics >> KMEditorMsg >> EditorMsg
            , addMsg = AddMetric |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "metric"
            }

        phasesConfig =
            { childName = lg "phase" appState
            , childNamePlural = lg "phases" appState
            , reorderableState = model.reorderableState
            , children = editorData.phases.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderPhases >> KMEditorMsg >> EditorMsg
            , addMsg = AddPhase |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "phase"
            }

        tagsConfig =
            { childName = lg "tag" appState
            , childNamePlural = lg "tags" appState
            , reorderableState = model.reorderableState
            , children = editorData.tags.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderTags >> KMEditorMsg >> EditorMsg
            , addMsg = AddTag |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "tag"
            }

        integrationsConfig =
            { childName = lg "integration" appState
            , childNamePlural = lg "integrations" appState
            , reorderableState = model.reorderableState
            , children = editorData.integrations.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderIntegrations >> KMEditorMsg >> EditorMsg
            , addMsg = AddIntegration |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "integration"
            }
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , inputChildren appState chaptersConfig
        , inputChildren appState metricsConfig
        , inputChildren appState phasesConfig
        , inputChildren appState tagsConfig
        , inputChildren appState integrationsConfig
        ]
    )


chapterEditorView : AppState -> String -> Model -> ChapterEditorData -> ( String, Html Msg )
chapterEditorView appState kmName model editorData =
    let
        editorTitleConfig =
            { title = lg "chapter" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteChapter editorData.uuid |> ChapterEditorMsg |> EditorMsg |> Just
            , movable = False
            }

        questionsConfig =
            { childName = lg "question" appState
            , childNamePlural = lg "questions" appState
            , reorderableState = model.reorderableState
            , children = editorData.questions.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderQuestions >> ChapterEditorMsg >> EditorMsg
            , addMsg = AddQuestion |> ChapterEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "question"
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "title" <| lg "chapter.title" appState
                , FormGroup.markdownEditor appState editorData.form "text" <| lg "chapter.text" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (ChapterFormMsg >> ChapterEditorMsg >> EditorMsg)
        , inputChildren appState questionsConfig
        ]
    )


metricEditorView : AppState -> Model -> MetricEditorData -> ( String, Html Msg )
metricEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "metric" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteMetric editorData.uuid |> MetricEditorMsg |> EditorMsg |> Just
            , movable = False
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "title" <| lg "metric.title" appState
                , FormGroup.input appState editorData.form "abbreviation" <| lg "metric.abbreviation" appState
                , FormGroup.textarea appState editorData.form "description" <| lg "metric.description" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (MetricFormMsg >> MetricEditorMsg >> EditorMsg)
        ]
    )


phaseEditorView : AppState -> Model -> PhaseEditorData -> ( String, Html Msg )
phaseEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "phase" appState
            , uuid = editorData.uuid
            , deleteAction = DeletePhase editorData.uuid |> PhaseEditorMsg |> EditorMsg |> Just
            , movable = False
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "title" <| lg "phase.title" appState
                , FormGroup.textarea appState editorData.form "description" <| lg "phase.description" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (PhaseFormMsg >> PhaseEditorMsg >> EditorMsg)
        ]
    )


tagEditorView : AppState -> Model -> TagEditorData -> ( String, Html Msg )
tagEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "tag" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteTag editorData.uuid |> TagEditorMsg |> EditorMsg |> Just
            , movable = False
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "name" <| lg "tag.name" appState
                , FormGroup.textarea appState editorData.form "description" <| lg "tag.description" appState
                , FormGroup.color appState editorData.form "color" <| lg "tag.color" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (TagFormMsg >> TagEditorMsg >> EditorMsg)
        ]
    )


integrationEditorView : AppState -> Model -> IntegrationEditorData -> ( String, Html Msg )
integrationEditorView appState model editorData =
    let
        formMsg =
            IntegrationFormMsg >> IntegrationEditorMsg >> EditorMsg

        propsListMsg =
            PropsListMsg >> IntegrationEditorMsg >> EditorMsg

        editorTitleConfig =
            { title = lg "integration" appState
            , uuid = editorData.uuid
            , deleteAction = Just <| EditorMsg <| IntegrationEditorMsg <| ToggleDeleteConfirm True
            , movable = False
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "id" (lg "integration.id" appState) |> Html.map formMsg
                , FormGroup.input appState editorData.form "name" (lg "integration.name" appState) |> Html.map formMsg
                , FormGroup.input appState editorData.form "logo" (lg "integration.logo" appState) |> Html.map formMsg
                , div [ class "form-group" ]
                    [ label [] [ lgx "integration.props" appState ]
                    , ValueList.view editorData.props |> Html.map propsListMsg
                    ]
                , FormGroup.input appState editorData.form "itemUrl" (lg "integration.itemUrl" appState) |> Html.map formMsg
                , div [ class "card card-border-light mb-5" ]
                    [ div [ class "card-header" ] [ lgx "integration.request" appState ]
                    , div [ class "card-body" ]
                        [ FormGroup.select appState httpMethodOptions editorData.form "requestMethod" (lg "integration.request.method" appState) |> Html.map formMsg
                        , FormGroup.input appState editorData.form "requestUrl" (lg "integration.request.url" appState) |> Html.map formMsg
                        , FormGroup.list appState (integrationHeaderItemView appState) editorData.form "requestHeaders" (lg "integration.request.headers" appState) |> Html.map formMsg
                        , FormGroup.textarea appState editorData.form "requestBody" (lg "integration.request.body" appState) |> Html.map formMsg
                        ]
                    ]
                , div [ class "card card-border-light mb-5" ]
                    [ div [ class "card-header" ] [ lgx "integration.response" appState ]
                    , div [ class "card-body" ]
                        [ FormGroup.input appState editorData.form "responseListField" (lg "integration.response.listField" appState) |> Html.map formMsg
                        , FormGroup.input appState editorData.form "responseIdField" (lg "integration.response.idField" appState) |> Html.map formMsg
                        , FormGroup.input appState editorData.form "responseNameField" (lg "integration.response.nameField" appState) |> Html.map formMsg
                        ]
                    ]
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form
        , integrationDeleteConfirm appState editorData
        ]
    )


integrationHeaderItemView : AppState -> Form FormError IntegrationForm -> Int -> Html Form.Msg
integrationHeaderItemView appState form i =
    let
        headerField =
            Form.getFieldAsString ("requestHeaders." ++ String.fromInt i ++ ".header") form

        valueField =
            Form.getFieldAsString ("requestHeaders." ++ String.fromInt i ++ ".value") form

        ( headerError, headerErrorClass ) =
            FormGroup.getErrors appState headerField <| lg "integration.header.name" appState

        ( valueError, valueErrorClass ) =
            FormGroup.getErrors appState valueField <| lg "integration.header.value" appState
    in
    div
        [ class "input-group mb-2"
        , dataCy "integration_headers_item"
        ]
        [ Input.textInput headerField
            [ class <| "form-control " ++ headerErrorClass
            , placeholder <| l_ "integrationEditor.form.header.namePlaceholder" appState
            , dataCy "integration_headers_name"
            ]
        , Input.textInput valueField
            [ class <| "form-control " ++ valueErrorClass
            , placeholder <| l_ "integrationEditor.form.header.valuePlaceholder" appState
            , dataCy "integration_headers_value"
            ]
        , div [ class "input-group-append" ]
            [ button
                [ class "btn btn-outline-warning"
                , onClick (Form.RemoveItem "requestHeaders" i)
                , dataCy "integration_headers_remove-button"
                ]
                [ faSet "_global.remove" appState ]
            ]
        , headerError
        , valueError
        ]


integrationDeleteConfirm : AppState -> IntegrationEditorData -> Html Msg
integrationDeleteConfirm appState editorData =
    Modal.confirm appState
        { modalTitle = l_ "integrationEditor.deleteConfirm.title" appState
        , modalContent =
            [ p [] [ lx_ "integrationEditor.deleteConfirm.text" appState ]
            ]
        , visible = editorData.deleteConfirmOpen
        , actionResult = Unset
        , actionName = l_ "integrationEditor.deleteConfirm.action" appState
        , actionMsg = EditorMsg <| IntegrationEditorMsg <| DeleteIntegration editorData.uuid
        , cancelMsg = Just <| EditorMsg <| IntegrationEditorMsg <| ToggleDeleteConfirm False
        , dangerous = True
        , dataCy = "km-editor-integration-delete"
        }


questionEditorView : AppState -> String -> Model -> QuestionEditorData -> ( String, Html Msg )
questionEditorView appState kmName model editorData =
    let
        editorTitleConfig =
            { title = lg "question" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteQuestion editorData.uuid |> QuestionEditorMsg |> EditorMsg |> Just
            , movable = True
            }

        levelSelection =
            questionRequiredPhaseSelectGroup appState editorData model

        formFields =
            [ FormGroup.select appState (questionTypeOptions appState) editorData.form "questionType" <| lg "question.type" appState
            , p [ class "form-text text-muted" ]
                [ faSet "_global.warning" appState
                , lx_ "questionEditor.form.questionTypeWarning" appState
                ]
            , FormGroup.input appState editorData.form "title" <| lg "question.title" appState
            , FormGroup.markdownEditor appState editorData.form "text" <| lg "question.text" appState
            , levelSelection
            ]

        ( form, extra ) =
            case (Form.getFieldAsString "questionType" editorData.form).value of
                Just "OptionsQuestion" ->
                    let
                        formData =
                            div [] formFields

                        extraData =
                            [ questionEditorAnswersView appState kmName model editorData ]
                    in
                    ( formData, extraData )

                Just "ListQuestion" ->
                    let
                        formData =
                            div [] formFields

                        extraData =
                            [ questionEditorItemView appState kmName model editorData ]
                    in
                    ( formData, extraData )

                Just "ValueQuestion" ->
                    let
                        formData =
                            div []
                                (formFields
                                    ++ [ FormGroup.select appState (questionValueTypeOptions appState) editorData.form "valueType" <| lg "questionValueType" appState
                                       ]
                                )

                        extraData =
                            []
                    in
                    ( formData, extraData )

                Just "IntegrationQuestion" ->
                    let
                        integrations =
                            getCurrentIntegrations model

                        integrationOptions =
                            ( "", l_ "questionEditor.form.integration.defaultValue" appState )
                                :: List.map (\i -> ( i.uuid, i.name )) integrations

                        noIntegrations =
                            if List.length integrations == 0 then
                                Flash.info appState <| l_ "questionEditor.form.integration.noIntegrations" appState

                            else
                                emptyNode

                        integrationFields integration =
                            if List.isEmpty integration.props then
                                emptyNode

                            else
                                div [ class "card card-border-light mb-5" ]
                                    [ div [ class "card-header" ] [ lx_ "questionEditor.form.integration.configuration" appState ]
                                    , div [ class "card-body" ]
                                        (List.map
                                            (\prop ->
                                                FormGroup.input appState editorData.form ("props-" ++ prop) prop
                                            )
                                            integration.props
                                        )
                                    ]

                        integrationFormFields =
                            (Form.getFieldAsString "integrationUuid" editorData.form).value
                                |> Maybe.andThen (\integrationUuid -> List.find (.uuid >> (==) integrationUuid) integrations)
                                |> Maybe.map integrationFields
                                |> Maybe.withDefault emptyNode

                        formData =
                            div [ class "integration-question-form" ]
                                (formFields
                                    ++ [ FormGroup.select appState integrationOptions editorData.form "integrationUuid" <| lg "integration" appState
                                       , noIntegrations
                                       , integrationFormFields
                                       ]
                                )

                        extraData =
                            []
                    in
                    ( formData, extraData )

                Just "MultiChoiceQuestion" ->
                    let
                        formData =
                            div [] formFields

                        extraData =
                            [ questionEditorChoicesView appState kmName model editorData ]
                    in
                    ( formData, extraData )

                _ ->
                    ( emptyNode, [] )
    in
    ( editorData.uuid
    , div [ class editorClass ]
        ([ editorTitle appState editorTitleConfig
         , form |> Html.map (QuestionFormMsg >> QuestionEditorMsg >> EditorMsg)
         , questionTagList appState model editorData
         ]
            ++ extra
            ++ [ questionEditorReferencesView appState kmName model editorData
               , questionEditorExpertsView appState kmName model editorData
               ]
        )
    )


questionTagList : AppState -> Model -> QuestionEditorData -> Html Msg
questionTagList appState model editorData =
    let
        tags =
            getCurrentTags model

        tagListConfig =
            { selected = editorData.tagUuids
            , addMsg = AddQuestionTag >> QuestionEditorMsg >> EditorMsg
            , removeMsg = RemoveQuestionTag >> QuestionEditorMsg >> EditorMsg
            }
    in
    div [ class "form-group" ]
        [ label [] [ lgx "tags" appState ]
        , Tag.list appState tagListConfig tags
        ]


questionRequiredPhaseSelectGroup : AppState -> QuestionEditorData -> Model -> Html Form.Msg
questionRequiredPhaseSelectGroup appState editorData model =
    let
        options =
            getCurrentPhases model
                |> List.map createPhaseOption
                |> (::) ( "", l_ "questionEditor.form.requiredLevel.defaultValue" appState )
    in
    FormGroup.select appState options editorData.form "requiredLevel" <| lg "question.requiredLevel" appState


createPhaseOption : Phase -> ( String, String )
createPhaseOption phase =
    ( phase.uuid, phase.title )


questionEditorAnswersView : AppState -> String -> Model -> QuestionEditorData -> Html Msg
questionEditorAnswersView appState kmName model editorData =
    inputChildren appState
        { childName = lg "answer" appState
        , childNamePlural = lg "answers" appState
        , reorderableState = model.reorderableState
        , children = editorData.answers.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderAnswers >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddAnswer |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName kmName model.editors
        , viewMsg = SetActiveEditor
        , dataCy = "answer"
        }


questionEditorChoicesView : AppState -> String -> Model -> QuestionEditorData -> Html Msg
questionEditorChoicesView appState kmName model editorData =
    inputChildren appState
        { childName = lg "choice" appState
        , childNamePlural = lg "choices" appState
        , reorderableState = model.reorderableState
        , children = editorData.choices.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderChoices >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddChoice |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName kmName model.editors
        , viewMsg = SetActiveEditor
        , dataCy = "choice"
        }


questionEditorItemView : AppState -> String -> Model -> QuestionEditorData -> Html Msg
questionEditorItemView appState kmName model editorData =
    let
        config =
            { childName = lg "question" appState
            , childNamePlural = lg "questions" appState
            , reorderableState = model.reorderableState
            , children = editorData.itemTemplateQuestions.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderItemQuestions >> QuestionEditorMsg >> EditorMsg
            , addMsg = AddAnswerItemTemplateQuestion |> QuestionEditorMsg >> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "question"
            }
    in
    div [ class "card card-border-light card-item-template mb-3" ]
        [ div [ class "card-header" ]
            [ lx_ "questionEditor.form.itemTemplate" appState ]
        , div [ class "card-body" ]
            [ inputChildren appState config
            ]
        ]


questionEditorReferencesView : AppState -> String -> Model -> QuestionEditorData -> Html Msg
questionEditorReferencesView appState kmName model editorData =
    inputChildren appState
        { childName = lg "reference" appState
        , childNamePlural = lg "references" appState
        , reorderableState = model.reorderableState
        , children = editorData.references.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderReferences >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddReference |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName kmName model.editors
        , viewMsg = SetActiveEditor
        , dataCy = "reference"
        }


questionEditorExpertsView : AppState -> String -> Model -> QuestionEditorData -> Html Msg
questionEditorExpertsView appState kmName model editorData =
    inputChildren appState
        { childName = lg "expert" appState
        , childNamePlural = lg "experts" appState
        , reorderableState = model.reorderableState
        , children = editorData.experts.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderExperts >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddExpert |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName kmName model.editors
        , viewMsg = SetActiveEditor
        , dataCy = "expert"
        }


answerEditorView : AppState -> String -> Model -> AnswerEditorData -> ( String, Html Msg )
answerEditorView appState kmName model editorData =
    let
        editorTitleConfig =
            { title = lg "answer" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteAnswer editorData.uuid |> AnswerEditorMsg |> EditorMsg |> Just
            , movable = True
            }

        metrics =
            getCurrentMetrics model

        viewMetrics =
            if List.length metrics > 0 then
                metricsView appState editorData metrics

            else
                FormGroup.plainGroup
                    (Flash.info appState (l_ "answerEditor.noMetrics" appState))
                    (lg "metrics" appState)

        followUpsConfig =
            { childName = lg "followupQuestion" appState
            , childNamePlural = lg "followupQuestions" appState
            , reorderableState = model.reorderableState
            , children = editorData.followUps.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderFollowUps >> AnswerEditorMsg >> EditorMsg
            , addMsg = AddFollowUp |> AnswerEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName kmName model.editors
            , viewMsg = SetActiveEditor
            , dataCy = "question"
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "label" <| lg "answer.label" appState
                , FormGroup.markdownEditor appState editorData.form "advice" <| lg "answer.advice" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (AnswerFormMsg >> AnswerEditorMsg >> EditorMsg)
        , inputChildren appState followUpsConfig
        , viewMetrics
        ]
    )


metricsView : AppState -> AnswerEditorData -> List Metric -> Html Msg
metricsView appState editorData metrics =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ lgx "metrics" appState ]
        , div [] (List.indexedMap (metricView appState editorData.form) metrics)
        ]
        |> Html.map (AnswerFormMsg >> AnswerEditorMsg >> EditorMsg)


metricView : AppState -> Form FormError AnswerForm -> Int -> Metric -> Html Form.Msg
metricView appState form i metric =
    let
        field name =
            "metricMeasure-" ++ metric.uuid ++ "-" ++ name

        enabled =
            Form.getFieldAsBool (field "enabled") form
                |> .value
                |> Maybe.withDefault False
    in
    div [ class "metric-view" ]
        [ FormGroup.toggle form (field "enabled") metric.title
        , div [ class "metric-view-inputs", classList [ ( "metric-view-inputs-enabled", enabled ) ] ]
            [ FormGroup.input appState form (field "weight") (lg "metric.weight" appState)
            , FormGroup.input appState form (field "measure") (lg "metric.measure" appState)
            ]
        ]


choiceEditorView : AppState -> ChoiceEditorData -> ( String, Html Msg )
choiceEditorView appState editorData =
    let
        editorTitleConfig =
            { title = lg "choice" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteChoice editorData.uuid |> ChoiceEditorMsg |> EditorMsg |> Just
            , movable = True
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "label" <| lg "choice.label" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (ChoiceFormMsg >> ChoiceEditorMsg >> EditorMsg)
        ]
    )


referenceEditorView : AppState -> ReferenceEditorData -> ( String, Html Msg )
referenceEditorView appState editorData =
    let
        editorTitleConfig =
            { title = lg "reference" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteReference editorData.uuid |> ReferenceEditorMsg |> EditorMsg |> Just
            , movable = True
            }

        formFields =
            case (Form.getFieldAsString "referenceType" editorData.form).value of
                Just "ResourcePageReference" ->
                    [ FormGroup.input appState editorData.form "shortUuid" <| lg "reference.shortUuid" appState
                    ]

                Just "URLReference" ->
                    [ FormGroup.input appState editorData.form "url" <| lg "reference.url" appState
                    , FormGroup.input appState editorData.form "label" <| lg "reference.label" appState
                    ]

                Just "CrossReference" ->
                    [ FormGroup.input appState editorData.form "targetUuid" <| lg "reference.targetUuid" appState
                    , FormGroup.input appState editorData.form "description" <| lg "reference.description" appState
                    ]

                _ ->
                    []

        form =
            div []
                ([ FormGroup.select appState (referenceTypeOptions appState) editorData.form "referenceType" <| lg "referenceType" appState ]
                    ++ formFields
                )
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (ReferenceFormMsg >> ReferenceEditorMsg >> EditorMsg)
        ]
    )


expertEditorView : AppState -> ExpertEditorData -> ( String, Html Msg )
expertEditorView appState editorData =
    let
        editorTitleConfig =
            { title = lg "expert" appState
            , uuid = editorData.uuid
            , deleteAction = DeleteExpert editorData.uuid |> ExpertEditorMsg |> EditorMsg |> Just
            , movable = True
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "name" <| lg "expert.name" appState
                , FormGroup.input appState editorData.form "email" <| lg "expert.email" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (ExpertFormMsg >> ExpertEditorMsg >> EditorMsg)
        ]
    )


type alias EditorTitleConfig =
    { title : String
    , uuid : String
    , deleteAction : Maybe Msg
    , movable : Bool
    }


editorTitle : AppState -> EditorTitleConfig -> Html Msg
editorTitle appState config =
    let
        copyUuidButton =
            button
                [ class "btn btn-link link-with-icon"
                , title <| l_ "editorTitle.copyUuid" appState
                , onClick <| CopyUuid config.uuid
                ]
                [ faSet "kmEditor.copyUuid" appState
                , small [] [ text <| String.slice 0 8 config.uuid ]
                ]

        moveActionButton =
            if config.movable then
                button
                    [ class "btn btn-outline-secondary link-with-icon"
                    , onClick OpenMoveModal
                    , dataCy "km-editor_move-button"
                    ]
                    [ faSet "kmEditor.move" appState
                    , lx_ "editorTitle.move" appState
                    ]

            else
                emptyNode

        deleteActionButton =
            case config.deleteAction of
                Just msg ->
                    button
                        [ class "btn btn-outline-danger link-with-icon"
                        , onClick msg
                        , dataCy "km-editor_delete-button"
                        ]
                        [ faSet "_global.delete" appState
                        , lx_ "editorTitle.delete" appState
                        ]

                Nothing ->
                    emptyNode
    in
    div [ class "editor-title" ]
        [ h3 [] [ text config.title ]
        , div [ class "editor-title-buttons" ]
            [ copyUuidButton
            , moveActionButton
            , deleteActionButton
            ]
        ]


type alias InputChildrenConfig a =
    { childName : String
    , childNamePlural : String
    , reorderableState : Reorderable.State
    , children : List a
    , reorderMsg : List a -> Msg
    , addMsg : Msg
    , toId : a -> String
    , getName : a -> String
    , viewMsg : a -> Msg
    , dataCy : String
    }


inputChildren : AppState -> InputChildrenConfig a -> Html Msg
inputChildren appState config =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ text config.childNamePlural ]
        , Reorderable.view
            { toId = config.toId
            , toMsg = ReorderableMsg
            , updateList = config.reorderMsg
            , itemView = inputChild config.getName config.viewMsg
            , placeholderView = placeholderView
            , listClass = "input-children"
            , itemClass = "input-child"
            , placeholderClass = "input-child input-child-placeholder"
            }
            config.reorderableState
            config.children
        , a
            [ onClick config.addMsg
            , class "link-with-icon link-add-child"
            , dataCy ("km-editor_input-children_" ++ config.dataCy ++ "_add-button")
            ]
            [ faSet "_global.add" appState
            , text <| lf_ "inputChildren.add" [ toLower config.childName ] appState
            ]
        ]


inputChild : (a -> String) -> (a -> Msg) -> Reorderable.HtmlWrapper Msg -> a -> Html Msg
inputChild getName viewMsg ignoreDrag item =
    div []
        [ ignoreDrag a
            [ onClick <| viewMsg item ]
            [ text <| getName item ]
        ]


placeholderView : Html msg
placeholderView =
    div [] [ text "-" ]
