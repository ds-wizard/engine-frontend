module Wizard.Pages.Settings.Common.Forms.EditableQuestionnairesConfigForm exposing
    ( EditableQuestionnairesConfigForm
    , init
    , initEmpty
    , toEditableQuestionnaireConfig
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Api.Models.EditableConfig.EditableProjectConfig exposing (EditableProjectConfig)
import Wizard.Api.Models.Project.ProjectCreation as ProjectCreation exposing (ProjectCreation)
import Wizard.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Wizard.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Wizard.Data.AppState exposing (AppState)


type alias EditableQuestionnairesConfigForm =
    { questionnaireVisibilityEnabled : Bool
    , questionnaireVisibilityDefaultValue : ProjectVisibility
    , questionnaireSharingEnabled : Bool
    , questionnaireSharingDefaultValue : ProjectSharing
    , questionnaireSharingAnonymousEnabled : Bool
    , questionnaireCreation : ProjectCreation
    , feedbackEnabled : Bool
    , feedbackToken : String
    , feedbackOwner : String
    , feedbackRepo : String
    , summaryReport : SimpleFeatureConfig
    , projectTaggingEnabled : Bool
    , projectTaggingTags : Maybe String
    }


initEmpty : AppState -> Form FormError EditableQuestionnairesConfigForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> EditableProjectConfig -> Form FormError EditableQuestionnairesConfigForm
init appState config =
    let
        fields =
            [ ( "questionnaireVisibilityEnabled", Field.bool config.projectVisibility.enabled )
            , ( "questionnaireVisibilityDefaultValue", ProjectVisibility.field config.projectVisibility.defaultValue )
            , ( "questionnaireSharingEnabled", Field.bool config.projectSharing.enabled )
            , ( "questionnaireSharingDefaultValue", ProjectSharing.field config.projectSharing.defaultValue )
            , ( "questionnaireSharingAnonymousEnabled", Field.bool config.projectSharing.anonymousEnabled )
            , ( "questionnaireCreation", ProjectCreation.field config.projectCreation )
            , ( "feedbackEnabled", Field.bool config.feedback.enabled )
            , ( "feedbackToken", Field.string config.feedback.token )
            , ( "feedbackOwner", Field.string config.feedback.owner )
            , ( "feedbackRepo", Field.string config.feedback.repo )
            , ( "summaryReport", SimpleFeatureConfig.field config.summaryReport )
            , ( "projectTaggingEnabled", Field.bool config.projectTagging.enabled )
            , ( "projectTaggingTags", Field.string <| String.join "\n" config.projectTagging.tags )
            ]
    in
    Form.initial fields (validation appState)


validation : AppState -> Validation FormError EditableQuestionnairesConfigForm
validation appState =
    V.succeed EditableQuestionnairesConfigForm
        |> V.andMap (V.field "questionnaireVisibilityEnabled" V.bool)
        |> V.andMap (V.field "questionnaireVisibilityDefaultValue" ProjectVisibility.validation)
        |> V.andMap (V.field "questionnaireSharingEnabled" V.bool)
        |> V.andMap (V.field "questionnaireSharingDefaultValue" ProjectSharing.validation)
        |> V.andMap (V.field "questionnaireSharingAnonymousEnabled" V.bool)
        |> V.andMap (V.field "questionnaireCreation" ProjectCreation.validation)
        |> V.andMap (V.field "feedbackEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackToken" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackOwner" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackRepo" V.string V.optionalString)
        |> V.andMap (V.field "summaryReport" SimpleFeatureConfig.validation)
        |> V.andMap (V.field "projectTaggingEnabled" V.bool)
        |> V.andMap (V.field "projectTaggingTags" (V.projectTags appState))


toEditableQuestionnaireConfig : EditableQuestionnairesConfigForm -> EditableProjectConfig
toEditableQuestionnaireConfig form =
    let
        tags =
            case form.projectTaggingTags of
                Just formTags ->
                    formTags
                        |> String.split "\n"
                        |> List.map String.trim
                        |> List.filter (not << String.isEmpty)

                Nothing ->
                    []
    in
    { projectVisibility =
        { enabled = form.questionnaireVisibilityEnabled
        , defaultValue = form.questionnaireVisibilityDefaultValue
        }
    , projectSharing =
        { enabled = form.questionnaireSharingEnabled
        , defaultValue = form.questionnaireSharingDefaultValue
        , anonymousEnabled = form.questionnaireSharingAnonymousEnabled
        }
    , projectCreation = form.questionnaireCreation
    , feedback =
        { enabled = form.feedbackEnabled
        , token = form.feedbackToken
        , owner = form.feedbackOwner
        , repo = form.feedbackRepo
        }
    , summaryReport = form.summaryReport
    , projectTagging =
        { enabled = form.projectTaggingEnabled
        , tags = tags
        }
    }
