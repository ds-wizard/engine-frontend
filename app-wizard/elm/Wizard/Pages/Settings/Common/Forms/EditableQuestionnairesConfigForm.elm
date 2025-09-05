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
import Wizard.Api.Models.EditableConfig.EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)
import Wizard.Api.Models.Questionnaire.QuestionnaireCreation as QuestionnaireCreation exposing (QuestionnaireCreation)
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Wizard.Data.AppState exposing (AppState)


type alias EditableQuestionnairesConfigForm =
    { questionnaireVisibilityEnabled : Bool
    , questionnaireVisibilityDefaultValue : QuestionnaireVisibility
    , questionnaireSharingEnabled : Bool
    , questionnaireSharingDefaultValue : QuestionnaireSharing
    , questionnaireSharingAnonymousEnabled : Bool
    , questionnaireCreation : QuestionnaireCreation
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


init : AppState -> EditableQuestionnairesConfig -> Form FormError EditableQuestionnairesConfigForm
init appState config =
    let
        fields =
            [ ( "questionnaireVisibilityEnabled", Field.bool config.questionnaireVisibility.enabled )
            , ( "questionnaireVisibilityDefaultValue", QuestionnaireVisibility.field config.questionnaireVisibility.defaultValue )
            , ( "questionnaireSharingEnabled", Field.bool config.questionnaireSharing.enabled )
            , ( "questionnaireSharingDefaultValue", QuestionnaireSharing.field config.questionnaireSharing.defaultValue )
            , ( "questionnaireSharingAnonymousEnabled", Field.bool config.questionnaireSharing.anonymousEnabled )
            , ( "questionnaireCreation", QuestionnaireCreation.field config.questionnaireCreation )
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
        |> V.andMap (V.field "questionnaireVisibilityDefaultValue" QuestionnaireVisibility.validation)
        |> V.andMap (V.field "questionnaireSharingEnabled" V.bool)
        |> V.andMap (V.field "questionnaireSharingDefaultValue" QuestionnaireSharing.validation)
        |> V.andMap (V.field "questionnaireSharingAnonymousEnabled" V.bool)
        |> V.andMap (V.field "questionnaireCreation" QuestionnaireCreation.validation)
        |> V.andMap (V.field "feedbackEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackToken" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackOwner" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackRepo" V.string V.optionalString)
        |> V.andMap (V.field "summaryReport" SimpleFeatureConfig.validation)
        |> V.andMap (V.field "projectTaggingEnabled" V.bool)
        |> V.andMap (V.field "projectTaggingTags" (V.projectTags appState))


toEditableQuestionnaireConfig : EditableQuestionnairesConfigForm -> EditableQuestionnairesConfig
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
    { questionnaireVisibility =
        { enabled = form.questionnaireVisibilityEnabled
        , defaultValue = form.questionnaireVisibilityDefaultValue
        }
    , questionnaireSharing =
        { enabled = form.questionnaireSharingEnabled
        , defaultValue = form.questionnaireSharingDefaultValue
        , anonymousEnabled = form.questionnaireSharingAnonymousEnabled
        }
    , questionnaireCreation = form.questionnaireCreation
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
