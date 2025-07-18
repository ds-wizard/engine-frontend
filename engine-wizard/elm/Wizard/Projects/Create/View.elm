module Wizard.Projects.Create.View exposing (view)

import ActionResult
import Form
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, p, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Shared.Components.FontAwesome exposing (faKnowledgeModel, faQuestionnaire)
import Wizard.Api.Models.PackageDetail as PackageDetail
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Components.TypeHintInput.TypeHintItem as TypeHintItem
import Wizard.Common.GuideLinks as GuideLinks
import Wizard.Common.Html.Attribute exposing (dataCy, detailClass)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.ActionResultBlock as ActionResultBlock
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.Common.View.Tag as Tag
import Wizard.Projects.Create.Models exposing (ActiveTab(..), DefaultMode(..), Mode(..), Model, mapMode)
import Wizard.Projects.Create.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        actionResult =
            [ ActionResult.map (always True) model.selectedProjectTemplate
            , ActionResult.map (always True) model.selectedKnowledgeModel
            , ActionResult.map (always True) model.anyProjectTemplates
            , ActionResult.map (always True) model.anyKnowledgeModels
            ]
                |> List.filter (not << ActionResult.isUnset)
                |> ActionResult.all
    in
    Page.actionResultView appState (viewPageContent appState model) actionResult


viewPageContent : AppState -> Model -> List a -> Html Msg
viewPageContent appState model _ =
    div [ detailClass "Projects__Create" ]
        [ Page.headerWithGuideLink appState (gettext "Create Project" appState.locale) GuideLinks.projectsCreate
        , FormResult.errorOnlyView model.savingQuestionnaire
        , formView appState model
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    div []
        [ Html.map FormMsg <| FormGroup.input appState model.form "name" <| gettext "Name" appState.locale
        , formContent appState model
        , FormActions.view appState
            Cancel
            (ActionButton.ButtonConfig (gettext "Create" appState.locale) model.savingQuestionnaire (FormMsg Form.Submit) False)
        ]


formContent : AppState -> Model -> Html Msg
formContent appState model =
    case model.mode of
        FromProjectTemplateMode ->
            formContentSelectedProjectTemplate appState model

        FromKnowledgeModelMode ->
            formContentSelectedKnowledgeModel appState model

        DefaultMode _ ->
            formContentDefault appState model


formContentSelectedProjectTemplate : AppState -> Model -> Html Msg
formContentSelectedProjectTemplate appState model =
    let
        viewProjectTemplate questionnaire =
            div [ class "bg-light px-2 py-1 rounded" ]
                [ TypeHintItem.questionnaireSuggestion questionnaire ]

        projectTemplateBlock =
            ActionResultBlock.inlineView appState viewProjectTemplate model.selectedProjectTemplate
    in
    FormGroup.plainGroup projectTemplateBlock (gettext "Project Template" appState.locale)


formContentSelectedKnowledgeModel : AppState -> Model -> Html Msg
formContentSelectedKnowledgeModel appState model =
    let
        viewKnowledgeModel packageDetail =
            div [ class "bg-light px-2 py-1 rounded" ]
                [ TypeHintItem.packageSuggestionWithVersion (PackageDetail.toPackageSuggestion packageDetail) ]

        viewFormContent ( packageDetail, _ ) =
            div []
                [ FormGroup.plainGroup (viewKnowledgeModel packageDetail) (gettext "Knowledge Model" appState.locale)
                , tagsView appState model
                ]
    in
    ActionResultBlock.inlineView appState viewFormContent (ActionResult.combine model.selectedKnowledgeModel model.knowledgeModelPreview)


formContentDefault : AppState -> Model -> Html Msg
formContentDefault appState model =
    let
        content =
            mapMode model
                (projectTemplateFormFields appState model)
                (knowledgeModelFormFields appState model)

        tabsEnabled =
            model.mode == DefaultMode TabsDefaultMode
    in
    div []
        [ Html.viewIf tabsEnabled <| defaultContentTabs appState model
        , Html.viewIf tabsEnabled <| defaultContentHint appState model
        , content
        ]


defaultContentTabs : AppState -> Model -> Html Msg
defaultContentTabs appState model =
    ul [ class "nav nav-underline-tabs nav-underline-tabs-full border-bottom" ]
        [ li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", model.activeTab == ProjectTemplateTab ) ]
                , onClick (SetActiveTab ProjectTemplateTab)
                , dataCy "project_create_nav_template"
                ]
                [ faQuestionnaire
                , text (gettext "From project template" appState.locale)
                ]
            ]
        , li [ class "nav-item" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", model.activeTab == KnowledgeModelTab ) ]
                , onClick (SetActiveTab KnowledgeModelTab)
                , dataCy "project_create_nav_custom"
                ]
                [ faKnowledgeModel
                , text (gettext "From knowledge model" appState.locale)
                ]
            ]
        ]


defaultContentHint : AppState -> Model -> Html Msg
defaultContentHint appState model =
    case model.activeTab of
        ProjectTemplateTab ->
            p [ class "form-text text-muted mt-3" ]
                [ text (gettext "Project templates are prepared projects with knowledge models, question tags, and document templates setup, so you don't have to start from scratch." appState.locale) ]

        KnowledgeModelTab ->
            p [ class "form-text text-muted mt-3" ]
                [ text (gettext "Knowledge models are templates for questionnaires. You can select question tags now and configure the document template and other settings later." appState.locale) ]


projectTemplateFormFields : AppState -> Model -> Html Msg
projectTemplateFormFields appState model =
    let
        projectTemplateInput =
            let
                cfg =
                    { viewItem = TypeHintItem.questionnaireSuggestion
                    , wrapMsg = ProjectTemplateTypeHintInputMsg
                    , nothingSelectedItem = text "--"
                    , clearEnabled = True
                    }

                typeHintInput =
                    TypeHintInput.view appState cfg model.projectTemplateTypeHintInputModel
            in
            FormGroup.formGroupCustom typeHintInput appState model.form "templateId" (gettext "Project Template" appState.locale)
    in
    div [] [ projectTemplateInput ]


knowledgeModelFormFields : AppState -> Model -> Html Msg
knowledgeModelFormFields appState model =
    let
        knowledgeModelInput =
            let
                cfg =
                    { viewItem = TypeHintItem.packageSuggestionWithVersion
                    , wrapMsg = KnowledgeModelTypeHintInputMsg
                    , nothingSelectedItem = text "--"
                    , clearEnabled = True
                    }

                typeHintInput =
                    TypeHintInput.view appState cfg model.knowledgeModelTypeHintInputModel
            in
            FormGroup.formGroupCustom typeHintInput appState model.form "packageId" (gettext "Knowledge Model" appState.locale)
    in
    div []
        [ knowledgeModelInput
        , tagsView appState model
        ]


tagsView : AppState -> Model -> Html Msg
tagsView appState model =
    let
        tagListConfig =
            { selected = model.selectedTags
            , addMsg = AddTag
            , removeMsg = RemoveTag
            , showDescription = True
            }

        selectionConfig =
            { tagListConfig = tagListConfig
            , useAllQuestions = model.useAllQuestions
            , useAllQuestionsMsg = ChangeUseAllQuestions
            }
    in
    Tag.selection appState selectionConfig model.knowledgeModelPreview
