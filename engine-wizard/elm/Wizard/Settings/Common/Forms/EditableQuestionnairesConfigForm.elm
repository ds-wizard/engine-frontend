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
    { questionnaireVisibilityEnabled : Bool
    , levelsEnabled : Bool
    , feedbackEnabled : Bool
    , feedbackToken : String
    , feedbackOwner : String
    , feedbackRepo : String
    }


initEmpty : Form CustomFormError EditableQuestionnairesConfigForm
initEmpty =
    Form.initial [] validation


init : EditableQuestionnairesConfig -> Form CustomFormError EditableQuestionnairesConfigForm
init config =
    let
        fields =
            [ ( "questionnaireVisibilityEnabled", Field.bool config.questionnaireVisibility.enabled )
            , ( "levelsEnabled", Field.bool config.levels.enabled )
            , ( "feedbackEnabled", Field.bool config.feedback.enabled )
            , ( "feedbackToken", Field.string config.feedback.token )
            , ( "feedbackOwner", Field.string config.feedback.owner )
            , ( "feedbackRepo", Field.string config.feedback.repo )
            ]
    in
    Form.initial fields validation


validation : Validation CustomFormError EditableQuestionnairesConfigForm
validation =
    V.succeed EditableQuestionnairesConfigForm
        |> V.andMap (V.field "questionnaireVisibilityEnabled" V.bool)
        |> V.andMap (V.field "levelsEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackToken" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackOwner" V.string V.optionalString)
        |> V.andMap (V.field "feedbackEnabled" V.bool |> V.ifElse "feedbackRepo" V.string V.optionalString)


toEditableQuestionnaireConfig : EditableQuestionnairesConfigForm -> EditableQuestionnairesConfig
toEditableQuestionnaireConfig form =
    { questionnaireVisibility = { enabled = form.questionnaireVisibilityEnabled }
    , levels = { enabled = form.levelsEnabled }
    , feedback =
        { enabled = form.feedbackEnabled
        , token = form.feedbackToken
        , owner = form.feedbackOwner
        , repo = form.feedbackRepo
        }
    }
