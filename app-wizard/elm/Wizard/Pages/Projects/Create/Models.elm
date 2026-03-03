module Wizard.Pages.Projects.Create.Models exposing
    ( ActiveTab(..)
    , DefaultMode(..)
    , Mode(..)
    , Model
    , initialModel
    , mapMode
    , updateDefaultMode
    )

import ActionResult exposing (ActionResult)
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Projects.Common.ProjectCreateForm as ProjectCreateForm exposing (ProjectCreateForm)
import Wizard.Utils.Feature as Feature


type alias Model =
    { selectedProjectTemplateUuid : Maybe Uuid
    , selectedKnowledgeModelUuid : Maybe Uuid
    , selectedProjectTemplate : ActionResult ProjectSettings
    , selectedKnowledgeModel : ActionResult KnowledgeModelPackageDetail
    , projectTemplateTypeHintInputModel : TypeHintInput.Model Project
    , knowledgeModelTypeHintInputModel : TypeHintInput.Model KnowledgeModelPackageSuggestion
    , anyProjectTemplates : ActionResult Bool
    , anyKnowledgeModels : ActionResult Bool
    , form : Form FormError ProjectCreateForm
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


initialModel : AppState -> Maybe Uuid -> Maybe Uuid -> Model
initialModel appState selectedProjectTemplateUuid selectedKnowledgeModelUuid =
    let
        mode =
            case ( selectedProjectTemplateUuid, selectedKnowledgeModelUuid ) of
                ( Just _, _ ) ->
                    FromProjectTemplateMode

                ( _, Just _ ) ->
                    FromKnowledgeModelMode

                _ ->
                    DefaultMode TabsDefaultMode

        knowledgeModelPreview =
            case selectedKnowledgeModelUuid of
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
            case selectedKnowledgeModelUuid of
                Just _ ->
                    ActionResult.Loading

                Nothing ->
                    ActionResult.Unset

        createFromTemplate =
            Feature.projectsCreateFromTemplate appState

        createCustom =
            Feature.projectsCreateCustom appState

        anythingPreselected =
            Maybe.isJust selectedProjectTemplateUuid || Maybe.isJust selectedKnowledgeModelUuid

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
    , selectedKnowledgeModelUuid = selectedKnowledgeModelUuid
    , selectedProjectTemplate = selectedProjectTemplate
    , selectedKnowledgeModel = selectedKnowledgeModel
    , projectTemplateTypeHintInputModel = TypeHintInput.init "projectUuid"
    , knowledgeModelTypeHintInputModel = TypeHintInput.init "knowledgeModelPackageUuid"
    , anyProjectTemplates = loadProjectTemplates
    , anyKnowledgeModels = loadKnowledgeModels
    , form = ProjectCreateForm.init appState ProjectCreateForm.TemplateValidationMode selectedProjectTemplateUuid selectedKnowledgeModelUuid
    , mode = mode
    , savingQuestionnaire = ActionResult.Unset
    , selectedTags = []
    , useAllQuestions = True
    , lastFetchedPreview = Maybe.map Uuid.toString selectedKnowledgeModelUuid
    , knowledgeModelPreview = knowledgeModelPreview
    , activeTab = ProjectTemplateTab
    }
