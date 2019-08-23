module KMEditor.Editor.KMEditor.View.Editors exposing (activeEditor)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Form exposing (CustomFormError)
import Common.Html exposing (emptyNode, fa)
import Common.Locale exposing (l, lf, lg, lgx, lx)
import Common.View.Flash as Flash
import Common.View.FormGroup as FormGroup
import Common.View.Modal as Modal
import Common.View.Page as Page
import Common.View.Tag as Tag
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, placeholder)
import Html.Events exposing (onClick)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KMEditor.Editor.KMEditor.Models exposing (Model, getActiveEditor, getCurrentIntegrations, getCurrentTags)
import KMEditor.Editor.KMEditor.Models.Editors exposing (..)
import KMEditor.Editor.KMEditor.Models.Forms exposing (AnswerForm, IntegrationForm, QuestionForm, questionTypeOptions, questionValueTypeOptions, referenceTypeOptions)
import KMEditor.Editor.KMEditor.Msgs exposing (..)
import List.Extra as List
import Reorderable
import String exposing (fromInt, toLower)
import ValueList


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.KMEditor.View.Editors"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "KMEditor.Editor.KMEditor.View.Editors"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KMEditor.Editor.KMEditor.View.Editors"


activeEditor : AppState -> Model -> ( String, Html Msg )
activeEditor appState model =
    case getActiveEditor model of
        Just editor ->
            case editor of
                KMEditor data ->
                    kmEditorView appState model data

                TagEditor data ->
                    tagEditorView appState model data

                IntegrationEditor data ->
                    integrationEditorView appState model data

                ChapterEditor data ->
                    chapterEditorView appState model data

                QuestionEditor data ->
                    questionEditorView appState model data

                AnswerEditor data ->
                    answerEditorView appState model data

                ReferenceEditor data ->
                    referenceEditorView appState data

                ExpertEditor data ->
                    expertEditorView appState data

        Nothing ->
            ( "nothing", Page.message "long-arrow-left" <| l_ "activeEditor.nothing" appState )


getChildName : Dict String Editor -> String -> String
getChildName editors uuid =
    Dict.get uuid editors
        |> Maybe.map getEditorTitle
        |> Maybe.withDefault "-"


editorClass : String
editorClass =
    "col-xl-10 col-lg-12"


kmEditorView : AppState -> Model -> KMEditorData -> ( String, Html Msg )
kmEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "knowledgeModel" appState
            , deleteAction = Nothing
            }

        chaptersConfig =
            { childName = lg "chapter" appState
            , childNamePlural = lg "chapters" appState
            , reorderableState = model.reorderableState
            , children = editorData.chapters.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderChapters >> KMEditorMsg >> EditorMsg
            , addMsg = AddChapter |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }

        tagsConfig =
            { childName = lg "tag" appState
            , childNamePlural = lg "tags" appState
            , reorderableState = model.reorderableState
            , children = editorData.tags.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderTags >> KMEditorMsg >> EditorMsg
            , addMsg = AddTag |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }

        integrationsConfig =
            { childName = lg "integration" appState
            , childNamePlural = lg "integrations" appState
            , reorderableState = model.reorderableState
            , children = editorData.integrations.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderIntegrations >> KMEditorMsg >> EditorMsg
            , addMsg = AddIntegration |> KMEditorMsg |> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "name" <| lg "knowledgeModel.name" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (KMEditorFormMsg >> KMEditorMsg >> EditorMsg)
        , inputChildren appState chaptersConfig
        , inputChildren appState tagsConfig
        , inputChildren appState integrationsConfig
        ]
    )


chapterEditorView : AppState -> Model -> ChapterEditorData -> ( String, Html Msg )
chapterEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "chapter" appState
            , deleteAction = DeleteChapter editorData.uuid |> ChapterEditorMsg |> EditorMsg |> Just
            }

        questionsConfig =
            { childName = lg "question" appState
            , childNamePlural = lg "questions" appState
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


tagEditorView : AppState -> Model -> TagEditorData -> ( String, Html Msg )
tagEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "tag" appState
            , deleteAction = DeleteTag editorData.uuid |> TagEditorMsg |> EditorMsg |> Just
            }

        form =
            div []
                [ FormGroup.input appState editorData.form "name" <| lg "tag.name" appState
                , FormGroup.textarea appState editorData.form "description" <| lg "tag.description" appState
                , FormGroup.color editorData.form "color" <| lg "tag.color" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (TagFormMsg >> TagEditorMsg >> EditorMsg)
        ]
    )


httpMethodOptions : List ( String, String )
httpMethodOptions =
    let
        httpMethods =
            [ "GET", "POST", "HEAD", "PUT", "DELETE", "OPTIONS", "PATCH" ]
    in
    List.zip httpMethods httpMethods


integrationEditorView : AppState -> Model -> IntegrationEditorData -> ( String, Html Msg )
integrationEditorView appState model editorData =
    let
        formMsg =
            IntegrationFormMsg >> IntegrationEditorMsg >> EditorMsg

        propsListMsg =
            PropsListMsg >> IntegrationEditorMsg >> EditorMsg

        editorTitleConfig =
            { title = lg "integration" appState
            , deleteAction = Just <| EditorMsg <| IntegrationEditorMsg <| ToggleDeleteConfirm True
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


integrationHeaderItemView : AppState -> Form CustomFormError IntegrationForm -> Int -> Html Form.Msg
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
    div [ class "input-group mb-2" ]
        [ Input.textInput headerField [ class <| "form-control " ++ headerErrorClass, placeholder <| l_ "integrationEditor.form.header.namePlaceholder" appState ]
        , Input.textInput valueField [ class <| "form-control " ++ valueErrorClass, placeholder <| l_ "integrationEditor.form.header.valuePlaceholder" appState ]
        , div [ class "input-group-append" ]
            [ button [ class "btn btn-outline-warning", onClick (Form.RemoveItem "requestHeaders" i) ]
                [ fa "times" ]
            ]
        , headerError
        , valueError
        ]


integrationDeleteConfirm : AppState -> IntegrationEditorData -> Html Msg
integrationDeleteConfirm appState editorData =
    Modal.confirm
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
        }


questionEditorView : AppState -> Model -> QuestionEditorData -> ( String, Html Msg )
questionEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "question" appState
            , deleteAction = DeleteQuestion editorData.uuid |> QuestionEditorMsg |> EditorMsg |> Just
            }

        levelSelection =
            if appState.config.levelsEnabled then
                questionRequiredLevelSelectGroup appState editorData model.levels

            else
                emptyNode

        formFields =
            [ FormGroup.select appState (questionTypeOptions appState) editorData.form "questionType" <| lg "questionType" appState
            , p [ class "form-text text-muted" ]
                [ fa "warning"
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
                            [ questionEditorAnswersView appState model editorData ]
                    in
                    ( formData, extraData )

                Just "ListQuestion" ->
                    let
                        formData =
                            div [] formFields

                        extraData =
                            [ questionEditorItemView appState model editorData ]
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
                                Flash.info <| l_ "questionEditor.form.integration.noIntegrations" appState

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
            ++ [ questionEditorReferencesView appState model editorData
               , questionEditorExpertsView appState model editorData
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


questionRequiredLevelSelectGroup : AppState -> QuestionEditorData -> List Level -> Html Form.Msg
questionRequiredLevelSelectGroup appState editorData levels =
    let
        options =
            levels
                |> List.map createLevelOption
                |> (::) ( "", l_ "questionEditor.form.requiredLevel.defaultValue" appState )
    in
    FormGroup.select appState options editorData.form "requiredLevel" <| lg "question.requiredLevel" appState


createLevelOption : Level -> ( String, String )
createLevelOption level =
    ( fromInt level.level, level.title )


questionEditorAnswersView : AppState -> Model -> QuestionEditorData -> Html Msg
questionEditorAnswersView appState model editorData =
    inputChildren appState
        { childName = lg "answer" appState
        , childNamePlural = lg "answers" appState
        , reorderableState = model.reorderableState
        , children = editorData.answers.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderAnswers >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddAnswer |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName model.editors
        , viewMsg = SetActiveEditor
        }


questionEditorItemView : AppState -> Model -> QuestionEditorData -> Html Msg
questionEditorItemView appState model editorData =
    let
        config =
            { childName = lg "question" appState
            , childNamePlural = lg "questions" appState
            , reorderableState = model.reorderableState
            , children = editorData.itemTemplateQuestions.list |> List.filter (editorNotDeleted model.editors)
            , reorderMsg = ReorderItemQuestions >> QuestionEditorMsg >> EditorMsg
            , addMsg = AddAnswerItemTemplateQuestion |> QuestionEditorMsg >> EditorMsg
            , toId = identity
            , getName = getChildName model.editors
            , viewMsg = SetActiveEditor
            }
    in
    div [ class "card card-border-light card-item-template mb-3" ]
        [ div [ class "card-header" ]
            [ lx_ "questionEditor.form.itemTemplate" appState ]
        , div [ class "card-body" ]
            [ inputChildren appState config
            ]
        ]


questionEditorReferencesView : AppState -> Model -> QuestionEditorData -> Html Msg
questionEditorReferencesView appState model editorData =
    inputChildren appState
        { childName = lg "reference" appState
        , childNamePlural = lg "references" appState
        , reorderableState = model.reorderableState
        , children = editorData.references.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderReferences >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddReference |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName model.editors
        , viewMsg = SetActiveEditor
        }


questionEditorExpertsView : AppState -> Model -> QuestionEditorData -> Html Msg
questionEditorExpertsView appState model editorData =
    inputChildren appState
        { childName = lg "expert" appState
        , childNamePlural = lg "experts" appState
        , reorderableState = model.reorderableState
        , children = editorData.experts.list |> List.filter (editorNotDeleted model.editors)
        , reorderMsg = ReorderExperts >> QuestionEditorMsg >> EditorMsg
        , addMsg = AddExpert |> QuestionEditorMsg |> EditorMsg
        , toId = identity
        , getName = getChildName model.editors
        , viewMsg = SetActiveEditor
        }


answerEditorView : AppState -> Model -> AnswerEditorData -> ( String, Html Msg )
answerEditorView appState model editorData =
    let
        editorTitleConfig =
            { title = lg "answer" appState
            , deleteAction = DeleteAnswer editorData.uuid |> AnswerEditorMsg |> EditorMsg |> Just
            }

        metrics =
            metricsView appState editorData model.metrics

        followUpsConfig =
            { childName = lg "followupQuestion" appState
            , childNamePlural = lg "followupQuestions" appState
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
                [ FormGroup.input appState editorData.form "label" <| lg "answer.label" appState
                , FormGroup.markdownEditor appState editorData.form "advice" <| lg "answer.advice" appState
                ]
    in
    ( editorData.uuid
    , div [ class editorClass ]
        [ editorTitle appState editorTitleConfig
        , form |> Html.map (AnswerFormMsg >> AnswerEditorMsg >> EditorMsg)
        , metrics
        , inputChildren appState followUpsConfig
        ]
    )


metricsView : AppState -> AnswerEditorData -> List Metric -> Html Msg
metricsView appState editorData metrics =
    div [ class "form-group" ]
        [ label [ class "control-label" ] [ lgx "metrics" appState ]
        , table [ class "table table-hover table-metrics" ]
            [ thead []
                [ tr []
                    [ th [] []
                    , th [] [ lgx "metric.weight" appState ]
                    , th [] [ lgx "metric.measure" appState ]
                    ]
                ]
            , tbody [] (List.indexedMap (metricView appState editorData.form) metrics)
            ]
        ]
        |> Html.map (AnswerFormMsg >> AnswerEditorMsg >> EditorMsg)


metricView : AppState -> Form CustomFormError AnswerForm -> Int -> Metric -> Html Form.Msg
metricView appState form i metric =
    let
        enabled =
            Form.getFieldAsBool ("metricMeasures." ++ fromInt i ++ ".enabled") form
                |> .value
                |> Maybe.withDefault False
    in
    tr [ classList [ ( "disabled", not enabled ) ] ]
        [ td [] [ FormGroup.toggle form ("metricMeasures." ++ fromInt i ++ ".enabled") metric.title ]
        , td [] [ metricInput appState form ("metricMeasures." ++ fromInt i ++ ".weight") enabled ]
        , td [] [ metricInput appState form ("metricMeasures." ++ fromInt i ++ ".measure") enabled ]
        ]


metricInput : AppState -> Form CustomFormError o -> String -> Bool -> Html Form.Msg
metricInput appState form fieldName enabled =
    FormGroup.formGroup Input.textInput [ disabled (not enabled) ] appState form fieldName ""


referenceEditorView : AppState -> ReferenceEditorData -> ( String, Html Msg )
referenceEditorView appState editorData =
    let
        editorTitleConfig =
            { title = lg "reference" appState
            , deleteAction = DeleteReference editorData.uuid |> ReferenceEditorMsg |> EditorMsg |> Just
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
            , deleteAction = DeleteExpert editorData.uuid |> ExpertEditorMsg |> EditorMsg |> Just
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
    , deleteAction : Maybe Msg
    }


editorTitle : AppState -> EditorTitleConfig -> Html Msg
editorTitle appState config =
    let
        deleteActionButton =
            case config.deleteAction of
                Just msg ->
                    button [ class "btn btn-outline-danger link-with-icon", onClick msg ]
                        [ fa "trash-o"
                        , lx_ "editorTitle.delete" appState
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
    , childNamePlural : String
    , reorderableState : Reorderable.State
    , children : List a
    , reorderMsg : List a -> Msg
    , addMsg : Msg
    , toId : a -> String
    , getName : a -> String
    , viewMsg : a -> Msg
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
        , a [ onClick config.addMsg, class "link-with-icon link-add-child" ]
            [ i [ class "fa fa-plus" ] []
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
