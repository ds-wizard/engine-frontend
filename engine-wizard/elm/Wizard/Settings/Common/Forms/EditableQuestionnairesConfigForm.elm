module Wizard.Settings.Common.Forms.EditableQuestionnairesConfigForm exposing
    ( EditableQuestionnairesConfigForm
    , init
    , initEmpty
    , toEditableQuestionnaireConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Shared.Data.EditableConfig.EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation exposing (QuestionnaireCreation)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias EditableQuestionnairesConfigForm =
    { questionnaireVisibilityEnabled : Bool
    , questionnaireVisibilityDefaultValue : QuestionnaireVisibility
    , questionnaireSharingEnabled : Bool
    , questionnaireSharingDefaultValue : QuestionnaireSharing
    , questionnaireSharingAnonymousEnabled : Bool
    , questionnaireCreation : QuestionnaireCreation
    , levels : SimpleFeatureConfig
    , feedbackEnabled : Bool
    , feedbackToken : String
    , feedbackOwner : String
    , feedbackRepo : String
    , summaryReport : SimpleFeatureConfig
    }


initEmpty : Form FormError EditableQuestionnairesConfigForm
initEmpty =
    Form.initial [] validation


init : EditableQuestionnairesConfig -> Form FormError EditableQuestionnairesConfigForm
init config =
    let
        fields =
            [ ( "questionnaireVisibilityEnabled", Field.bool config.questionnaireVisibility.enabled )
            , ( "questionnaireVisibilityDefaultValue", QuestionnaireVisibility.field config.questionnaireVisibility.defaultValue )
            , ( "questionnaireSharingEnabled", Field.bool config.questionnaireSharing.enabled )
            , ( "questionnaireSharingDefaultValue", QuestionnaireSharing.field config.questionnaireSharing.defaultValue )
            , ( "questionnaireSharingAnonymousEnabled", Field.bool config.questionnaireSharing.anonymousEnabled )
            , ( "questionnaireCreation", QuestionnaireCreation.field config.questionnaireCreation )
            , ( "levels", SimpleFeatureConfig.field config.phases )
            , ( "feedbackEnabled", Field.bool config.feedback.enabled )
            , ( "feedbackToken", Field.string config.feedback.token )
            , ( "feedbackOwner", Field.string config.feedback.owner )
            , ( "feedbackRepo", Field.string config.feedback.repo )
            , ( "summaryReport", SimpleFeatureConfig.field config.summaryReport )
            ]
    in
    Form.initial fields validation


validation : Validation FormError EditableQuestionnairesConfigForm
validation =
    V.succeed EditableQuestionnairesConfigForm
        |> V.andMap (V.field "questionnaireVisibilityEnabled" V.bool)
        |> V.andMap (V.field "questionnaireVisibilityDefaultValue" QuestionnaireVisibility.validation)
        |> V.andMap (V.field "questionnaireSharingEnabled" V.bool)
        |> V.andMap (V.field "questionnaireSharingDefaultValue" QuestionnaireSharing.validation)
        |> V.andMap (V.field "questionnaireSharingAnonymousEnabled" V.bool)
        |> V.andMap (V.field "questionnaireCreation" QuestionnaireCreation.validation)
        |> V.andMap (V.field "levels" SimpleFeatureConfig.validation)
        |> V.andMap (V.field "feedbackEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackToken" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackOwner" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackRepo" V.string V.optionalString)
        |> V.andMap (V.field "summaryReport" SimpleFeatureConfig.validation)


toEditableQuestionnaireConfig : EditableQuestionnairesConfigForm -> EditableQuestionnairesConfig
toEditableQuestionnaireConfig form =
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
    , phases = form.levels
    , feedback =
        { enabled = form.feedbackEnabled
        , token = form.feedbackToken
        , owner = form.feedbackOwner
        , repo = form.feedbackRepo
        }
    , summaryReport = form.summaryReport
    }
