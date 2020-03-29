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
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V
import Wizard.Settings.Common.EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)


type alias EditableQuestionnairesConfigForm =
    { questionnaireAccessibilityEnabled : Bool
    , levelsEnabled : Bool
    , feedbackEnabled : Bool
    , feedbackToken : String
    , feedbackOwner : String
    , feedbackRepo : String
    , publicQuestionnaireEnabled : Bool
    }


initEmpty : Form CustomFormError EditableQuestionnairesConfigForm
initEmpty =
    Form.initial [] validation


init : EditableQuestionnairesConfig -> Form CustomFormError EditableQuestionnairesConfigForm
init config =
    let
        fields =
            [ ( "questionnaireAccessibilityEnabled", Field.bool config.questionnaireAccessibility.enabled )
            , ( "levelsEnabled", Field.bool config.levels.enabled )
            , ( "feedbackEnabled", Field.bool config.feedback.enabled )
            , ( "feedbackToken", Field.string config.feedback.token )
            , ( "feedbackOwner", Field.string config.feedback.owner )
            , ( "feedbackRepo", Field.string config.feedback.repo )
            , ( "publicQuestionnaireEnabled", Field.bool config.publicQuestionnaire.enabled )
            ]
    in
    Form.initial fields validation


validation : Validation CustomFormError EditableQuestionnairesConfigForm
validation =
    V.succeed EditableQuestionnairesConfigForm
        |> V.andMap (V.field "questionnaireAccessibilityEnabled" V.bool)
        |> V.andMap (V.field "levelsEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackToken" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackOwner" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackRepo" V.string V.optionalString)
        |> V.andMap (V.field "publicQuestionnaireEnabled" V.bool)


toEditableQuestionnaireConfig : EditableQuestionnairesConfigForm -> EditableQuestionnairesConfig
toEditableQuestionnaireConfig form =
    { questionnaireAccessibility = { enabled = form.questionnaireAccessibilityEnabled }
    , levels = { enabled = form.levelsEnabled }
    , feedback =
        { enabled = form.feedbackEnabled
        , token = form.feedbackToken
        , owner = form.feedbackOwner
        , repo = form.feedbackRepo
        }
    , publicQuestionnaire = { enabled = form.publicQuestionnaireEnabled }
    }
