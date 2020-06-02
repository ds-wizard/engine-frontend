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
import Wizard.Common.Config.Partials.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V
import Wizard.Questionnaires.Common.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Wizard.Settings.Common.EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)


type alias EditableQuestionnairesConfigForm =
    { questionnaireVisibilityEnabled : Bool
    , questionnaireVisibilityDefaultValue : QuestionnaireVisibility
    , levels : SimpleFeatureConfig
    , feedbackEnabled : Bool
    , feedbackToken : String
    , feedbackOwner : String
    , feedbackRepo : String
    , summaryReport : SimpleFeatureConfig
    }


initEmpty : Form CustomFormError EditableQuestionnairesConfigForm
initEmpty =
    Form.initial [] validation


init : EditableQuestionnairesConfig -> Form CustomFormError EditableQuestionnairesConfigForm
init config =
    let
        fields =
            [ ( "questionnaireVisibilityEnabled", Field.bool config.questionnaireVisibility.enabled )
            , ( "questionnaireVisibilityDefaultValue", QuestionnaireVisibility.field config.questionnaireVisibility.defaultValue )
            , ( "levels", SimpleFeatureConfig.field config.levels )
            , ( "feedbackEnabled", Field.bool config.feedback.enabled )
            , ( "feedbackToken", Field.string config.feedback.token )
            , ( "feedbackOwner", Field.string config.feedback.owner )
            , ( "feedbackRepo", Field.string config.feedback.repo )
            , ( "summaryReport", SimpleFeatureConfig.field config.summaryReport )
            ]
    in
    Form.initial fields validation


validation : Validation CustomFormError EditableQuestionnairesConfigForm
validation =
    V.succeed EditableQuestionnairesConfigForm
        |> V.andMap (V.field "questionnaireVisibilityEnabled" V.bool)
        |> V.andMap (V.field "questionnaireVisibilityDefaultValue" QuestionnaireVisibility.validation)
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
    , levels = form.levels
    , feedback =
        { enabled = form.feedbackEnabled
        , token = form.feedbackToken
        , owner = form.feedbackOwner
        , repo = form.feedbackRepo
        }
    , summaryReport = form.summaryReport
    }
