module Wizard.Projects.Create.Models exposing
    ( ActiveTab(..)
    , DefaultMode(..)
    , Mode(..)
    , Model
    , initialModel
    , mapMode
    , updateDefaultMode
    )

import ActionResult exposing (ActionResult)
import Form exposing (Form)
import Maybe.Extra as Maybe
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Common.Feature as Feature
import Wizard.Projects.Common.QuestionnaireCreateForm as QuestionnaireCreateForm exposing (QuestionnaireCreateForm)


type alias Model =
    { selectedProjectTemplateUuid : Maybe Uuid
    , selectedKnowledgeModelId : Maybe String
    , selectedProjectTemplate : ActionResult QuestionnaireDetail
    , selectedKnowledgeModel : ActionResult PackageDetail
    , projectTemplateTypeHintInputModel : TypeHintInput.Model Questionnaire
    , knowledgeModelTypeHintInputModel : TypeHintInput.Model PackageSuggestion
    , anyProjectTemplates : ActionResult Bool
    , anyKnowledgeModels : ActionResult Bool
    , form : Form FormError QuestionnaireCreateForm
    , mode : Mode
    , savingQuestionnaire : ActionResult ()
    , selectedTags : List String
    , useAllQuestions : Bool
    , lastFetchedPreview : Maybe String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , activeTab : ActiveTab
    }


type Mode
    = FromProjectTemplateMode
    | FromKnowledgeModelMode
    | DefaultMode DefaultMode


type DefaultMode
    = TabsDefaultMode
    | ProjectTemplateDefaultMode
    | KnowledgeModelDefaultMode


type ActiveTab
    = ProjectTemplateTab
    | KnowledgeModelTab


mapMode : Model -> a -> a -> a
mapMode model fromProjectTemplateMode fromKnowledgeModelMode =
    case model.mode of
        FromProjectTemplateMode ->
            fromProjectTemplateMode

        FromKnowledgeModelMode ->
            fromKnowledgeModelMode

        DefaultMode defaultMode ->
            case defaultMode of
                ProjectTemplateDefaultMode ->
                    fromProjectTemplateMode

                KnowledgeModelDefaultMode ->
                    fromKnowledgeModelMode

                TabsDefaultMode ->
                    case model.activeTab of
                        ProjectTemplateTab ->
                            fromProjectTemplateMode

                        KnowledgeModelTab ->
                            fromKnowledgeModelMode


updateDefaultMode : AppState -> Model -> Model
updateDefaultMode appState model =
    let
        createFromProjectTemplates =
            Feature.projectsCreateFromTemplate appState

        createFromKnowledgeModels =
            Feature.projectsCreateCustom appState
    in
    if createFromProjectTemplates && not createFromKnowledgeModels then
        { model | mode = DefaultMode ProjectTemplateDefaultMode }

    else if not createFromProjectTemplates && createFromKnowledgeModels then
        { model | mode = DefaultMode KnowledgeModelDefaultMode }

    else
        case ( model.anyKnowledgeModels, model.anyProjectTemplates ) of
            ( ActionResult.Success True, ActionResult.Success False ) ->
                { model | mode = DefaultMode KnowledgeModelDefaultMode }

            ( ActionResult.Success False, ActionResult.Success True ) ->
                { model | mode = DefaultMode ProjectTemplateDefaultMode }

            _ ->
                { model | mode = DefaultMode TabsDefaultMode }


initialModel : AppState -> Maybe Uuid -> Maybe String -> Model
initialModel appState selectedProjectTemplateUuid selectedKnowledgeModelId =
    let
        mode =
            case ( selectedProjectTemplateUuid, selectedKnowledgeModelId ) of
                ( Just _, _ ) ->
                    FromProjectTemplateMode

                ( _, Just _ ) ->
                    FromKnowledgeModelMode

                _ ->
                    DefaultMode TabsDefaultMode

        knowledgeModelPreview =
            case selectedKnowledgeModelId of
                Just _ ->
                    ActionResult.Loading

                Nothing ->
                    ActionResult.Unset

        selectedProjectTemplate =
            case selectedProjectTemplateUuid of
                Just _ ->
                    ActionResult.Loading

                Nothing ->
                    ActionResult.Unset

        selectedKnowledgeModel =
            case selectedKnowledgeModelId of
                Just _ ->
                    ActionResult.Loading

                Nothing ->
                    ActionResult.Unset

        createFromTemplate =
            Feature.projectsCreateFromTemplate appState

        createCustom =
            Feature.projectsCreateCustom appState

        anythingPreselected =
            Maybe.isJust selectedProjectTemplateUuid || Maybe.isJust selectedKnowledgeModelId

        loadProjectTemplates =
            if createFromTemplate && not anythingPreselected then
                ActionResult.Loading

            else
                ActionResult.Unset

        loadKnowledgeModels =
            if createCustom && not anythingPreselected then
                ActionResult.Loading

            else
                ActionResult.Unset
    in
    { selectedProjectTemplateUuid = selectedProjectTemplateUuid
    , selectedKnowledgeModelId = selectedKnowledgeModelId
    , selectedProjectTemplate = selectedProjectTemplate
    , selectedKnowledgeModel = selectedKnowledgeModel
    , projectTemplateTypeHintInputModel = TypeHintInput.init "templateId"
    , knowledgeModelTypeHintInputModel = TypeHintInput.init "packageId"
    , anyProjectTemplates = loadProjectTemplates
    , anyKnowledgeModels = loadKnowledgeModels
    , form = QuestionnaireCreateForm.init appState QuestionnaireCreateForm.TemplateValidationMode selectedProjectTemplateUuid selectedKnowledgeModelId
    , mode = mode
    , savingQuestionnaire = ActionResult.Unset
    , selectedTags = []
    , useAllQuestions = True
    , lastFetchedPreview = selectedKnowledgeModelId
    , knowledgeModelPreview = knowledgeModelPreview
    , activeTab = ProjectTemplateTab
    }
