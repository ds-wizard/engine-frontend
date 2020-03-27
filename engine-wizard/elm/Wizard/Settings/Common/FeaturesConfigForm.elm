module Wizard.Settings.Common.FeaturesConfigForm exposing
    ( FeaturesConfigForm
    , init
    , initEmpty
    , toEditableFeaturesConfig
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Settings.Common.EditableFeaturesConfig exposing (EditableFeaturesConfig)


type alias FeaturesConfigForm =
    { levelsEnabled : Bool
    , publicQuestionnaireEnabled : Bool
    , questionnaireAccessibilityEnabled : Bool
    }


initEmpty : Form CustomFormError FeaturesConfigForm
initEmpty =
    Form.initial [] validation


init : EditableFeaturesConfig -> Form CustomFormError FeaturesConfigForm
init config =
    Form.initial (featuresConfigToFormInitials config) validation


validation : Validation CustomFormError FeaturesConfigForm
validation =
    V.succeed FeaturesConfigForm
        |> V.andMap (V.field "levelsEnabled" V.bool)
        |> V.andMap (V.field "publicQuestionnaireEnabled" V.bool)
        |> V.andMap (V.field "questionnaireAccessibilityEnabled" V.bool)


featuresConfigToFormInitials : EditableFeaturesConfig -> List ( String, Field.Field )
featuresConfigToFormInitials config =
    [ ( "levelsEnabled", Field.bool config.levels.enabled )
    , ( "publicQuestionnaireEnabled", Field.bool config.publicQuestionnaire.enabled )
    , ( "questionnaireAccessibilityEnabled", Field.bool config.questionnaireAccessibility.enabled )
    ]


toEditableFeaturesConfig : FeaturesConfigForm -> EditableFeaturesConfig
toEditableFeaturesConfig form =
    { levels = { enabled = form.levelsEnabled }
    , publicQuestionnaire = { enabled = form.publicQuestionnaireEnabled }
    , questionnaireAccessibility = { enabled = form.questionnaireAccessibilityEnabled }
    }
