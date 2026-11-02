module Common.Data.Forms.ProjectsSettingsForm exposing
    ( ProjectsSettingsForm
    , init
    , initEmpty
    , toSettings
    , validation
    )

import Common.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Common.Api.Models.Project.ProjectCreation as ProjectCreation exposing (ProjectCreation)
import Common.Api.Models.Project.ProjectSharing as ProjectSharing exposing (ProjectSharing)
import Common.Api.Models.Project.ProjectVisibility as ProjectVisibility exposing (ProjectVisibility)
import Common.Api.Models.Settings.ProjectsSettings exposing (ProjectsSettings)
import Common.Data.AppState exposing (AppState)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import List.Extra as List


type alias ProjectsSettingsForm =
    { questionnaireVisibilityEnabled : Bool
    , questionnaireVisibilityDefaultValue : ProjectVisibility
    , questionnaireSharingEnabled : Bool
    , questionnaireSharingDefaultValue : ProjectSharing
    , questionnaireSharingAnonymousEnabled : Bool
    , questionnaireCreation : ProjectCreation
    , summaryReport : SimpleFeatureConfig
    , projectTaggingEnabled : Bool
    , projectTaggingTags : Maybe String
    }


initEmpty : AppState -> Form FormError ProjectsSettingsForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> ProjectsSettings -> Form FormError ProjectsSettingsForm
init appState config =
    let
        fields =
            [ ( "questionnaireVisibilityEnabled", Field.bool config.projectVisibility.enabled )
            , ( "questionnaireVisibilityDefaultValue", ProjectVisibility.field config.projectVisibility.defaultValue )
            , ( "questionnaireSharingEnabled", Field.bool config.projectSharing.enabled )
            , ( "questionnaireSharingDefaultValue", ProjectSharing.field config.projectSharing.defaultValue )
            , ( "questionnaireSharingAnonymousEnabled", Field.bool config.projectSharing.anonymousEnabled )
            , ( "questionnaireCreation", ProjectCreation.field config.projectCreation )
            , ( "summaryReport", SimpleFeatureConfig.field config.summaryReport )
            , ( "projectTaggingEnabled", Field.bool config.projectTagging.enabled )
            , ( "projectTaggingTags", Field.string <| String.join "\n" config.projectTagging.tags )
            ]
    in
    Form.initial fields (validation appState)


validation : AppState -> Validation FormError ProjectsSettingsForm
validation appState =
    V.succeed ProjectsSettingsForm
        |> V.andMap (V.field "questionnaireVisibilityEnabled" V.bool)
        |> V.andMap (V.field "questionnaireVisibilityDefaultValue" ProjectVisibility.validation)
        |> V.andMap (V.field "questionnaireSharingEnabled" V.bool)
        |> V.andMap (V.field "questionnaireSharingDefaultValue" ProjectSharing.validation)
        |> V.andMap (V.field "questionnaireSharingAnonymousEnabled" V.bool)
        |> V.andMap (V.field "questionnaireCreation" ProjectCreation.validation)
        |> V.andMap (V.field "summaryReport" SimpleFeatureConfig.validation)
        |> V.andMap (V.field "projectTaggingEnabled" V.bool)
        |> V.andMap (V.field "projectTaggingTags" (V.projectTags appState))


toSettings : ProjectsSettingsForm -> ProjectsSettings
toSettings form =
    let
        tags =
            case form.projectTaggingTags of
                Just formTags ->
                    formTags
                        |> String.split "\n"
                        |> List.map String.trim
                        |> List.filterNot String.isEmpty

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
    , summaryReport = form.summaryReport
    , projectTagging =
        { enabled = form.projectTaggingEnabled
        , tags = tags
        }
    }
