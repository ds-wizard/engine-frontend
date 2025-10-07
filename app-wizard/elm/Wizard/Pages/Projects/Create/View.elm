module Wizard.Pages.Projects.Create.View exposing (view)

import ActionResult
import Common.Components.ActionResultBlock as ActionResultBlock
import Common.Components.Container as Container
import Common.Components.FontAwesome exposing (faKnowledgeModel, faQuestionnaire)
import Common.Components.Form as Form
import Common.Components.FormGroup as FormGroup
import Common.Components.Page as Page
import Common.Components.TypeHintInput as TypeHintInput
import Gettext exposing (gettext)
import Html exposing (Html, button, div, li, p, text, ul)
import Html.Attributes exposing (class, classList, tabindex)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Wizard.Api.Models.PackageDetail as PackageDetail
import Wizard.Components.Tag as Tag
import Wizard.Components.TypeHintInput.TypeHintInputItem as TypeHintInputItem
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Projects.Create.Models exposing (ActiveTab(..), DefaultMode(..), Mode(..), Model, mapMode)
import Wizard.Pages.Projects.Create.Msgs exposing (Msg(..))
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


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
    Container.simpleForm
        [ Page.headerWithGuideLink
            (AppState.toGuideLinkConfig appState WizardGuideLinks.projectsCreate)
            (gettext "Create Project" appState.locale)
        , Form.viewSimple
            { formMsg = FormMsg
            , formResult = model.savingQuestionnaire
            , formView = formView appState model
            , submitLabel = gettext "Create" appState.locale
            , cancelMsg = Just Cancel
            , locale = appState.locale
            , isMac = appState.navigator.isMac
            }
        ]


formView : AppState -> Model -> Html Msg
formView appState model =
    div []
        [ Html.map FormMsg <| FormGroup.input appState.locale model.form "name" <| gettext "Name" appState.locale
        , formContent appState model
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
                [ TypeHintInputItem.questionnaireSuggestion questionnaire ]

        projectTemplateBlock =
            ActionResultBlock.inlineView
                { viewContent = viewProjectTemplate
                , actionResult = model.selectedProjectTemplate
                , locale = appState.locale
                }
    in
    FormGroup.plainGroup projectTemplateBlock (gettext "Project Template" appState.locale)


formContentSelectedKnowledgeModel : AppState -> Model -> Html Msg
formContentSelectedKnowledgeModel appState model =
    let
        viewKnowledgeModel packageDetail =
            div [ class "bg-light px-2 py-1 rounded" ]
                [ TypeHintInputItem.packageSuggestionWithVersion (PackageDetail.toPackageSuggestion packageDetail) ]

        viewFormContent ( packageDetail, _ ) =
            div []
                [ FormGroup.plainGroup (viewKnowledgeModel packageDetail) (gettext "Knowledge Model" appState.locale)
                , tagsView appState model
                ]
    in
    ActionResultBlock.inlineView
        { viewContent = viewFormContent
        , actionResult = ActionResult.combine model.selectedKnowledgeModel model.knowledgeModelPreview
        , locale = appState.locale
        }


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
            [ button
                [ class "nav-link"
                , classList [ ( "active", model.activeTab == ProjectTemplateTab ) ]
                , onClick (SetActiveTab ProjectTemplateTab)
                , tabindex 0
                , dataCy "project_create_nav_template"
                ]
                [ faQuestionnaire
                , text (gettext "From project template" appState.locale)
                ]
            ]
        , li [ class "nav-item" ]
            [ button
                [ class "nav-link"
                , classList [ ( "active", model.activeTab == KnowledgeModelTab ) ]
                , onClick (SetActiveTab KnowledgeModelTab)
                , tabindex 0
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
                    { viewItem = TypeHintInputItem.questionnaireSuggestion
                    , wrapMsg = ProjectTemplateTypeHintInputMsg
                    , nothingSelectedItem = text "--"
                    , clearEnabled = True
                    , locale = appState.locale
                    }

                typeHintInput =
                    TypeHintInput.view cfg model.projectTemplateTypeHintInputModel
            in
            FormGroup.formGroupCustom typeHintInput appState.locale model.form "templateId" (gettext "Project Template" appState.locale)
    in
    div [] [ projectTemplateInput ]


knowledgeModelFormFields : AppState -> Model -> Html Msg
knowledgeModelFormFields appState model =
    let
        knowledgeModelInput =
            let
                cfg =
                    { viewItem = TypeHintInputItem.packageSuggestionWithVersion
                    , wrapMsg = KnowledgeModelTypeHintInputMsg
                    , nothingSelectedItem = text "--"
                    , clearEnabled = True
                    , locale = appState.locale
                    }

                typeHintInput =
                    TypeHintInput.view cfg model.knowledgeModelTypeHintInputModel
            in
            FormGroup.formGroupCustom typeHintInput appState.locale model.form "packageId" (gettext "Knowledge Model" appState.locale)
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
