module Wizard.Questionnaires.Common.QuestionnaireEditForm exposing
    ( QuestionnaireEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Shared.Form.FormError exposing (FormError)


type alias QuestionnaireEditForm =
    { name : String
    , visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    }


initEmpty : Form FormError QuestionnaireEditForm
initEmpty =
    Form.initial [] validation


init : QuestionnaireDetail -> Form FormError QuestionnaireEditForm
init questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) validation


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    let
        ( visibilityEnabled, visibilityPermission ) =
            QuestionnaireVisibility.toFormValues questionnaire.visibility

        ( sharingEnabled, sharingPermission ) =
            QuestionnaireSharing.toFormValues questionnaire.sharing
    in
    [ ( "name", Field.string questionnaire.name )
    , ( "visibilityEnabled", Field.bool visibilityEnabled )
    , ( "visibilityPermission", QuestionnairePermission.field visibilityPermission )
    , ( "sharingEnabled", Field.bool sharingEnabled )
    , ( "sharingPermission", QuestionnairePermission.field sharingPermission )
    ]


validation : Validation FormError QuestionnaireEditForm
validation =
    Validate.map5 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "visibilityEnabled" Validate.bool)
        (Validate.field "visibilityPermission" QuestionnairePermission.validation)
        (Validate.field "sharingEnabled" Validate.bool)
        (Validate.field "sharingPermission" QuestionnairePermission.validation)


encode : QuestionnaireEditForm -> E.Value
encode form =
    E.object
        [ ( "name", E.string form.name )
        , ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        ]
