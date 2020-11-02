module Wizard.Projects.Common.QuestionnaireEditForm exposing
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
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Shared.Data.Permission as Permission exposing (Permission)
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing exposing (QuestionnaireSharing)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Shared.Form.FormError exposing (FormError)
import Uuid


type alias QuestionnaireEditForm =
    { name : String
    , visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    , templateId : Maybe String
    , formatUuid : Maybe String
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
    , ( "templateId", Field.string (Maybe.withDefault "" questionnaire.templateId) )
    , ( "formatUuid", Field.string (Maybe.unwrap "" Uuid.toString questionnaire.formatUuid) )
    ]


validation : Validation FormError QuestionnaireEditForm
validation =
    Validate.map7 QuestionnaireEditForm
        (Validate.field "name" Validate.string)
        (Validate.field "visibilityEnabled" Validate.bool)
        (Validate.field "visibilityPermission" QuestionnairePermission.validation)
        (Validate.field "sharingEnabled" Validate.bool)
        (Validate.field "sharingPermission" QuestionnairePermission.validation)
        (Validate.field "templateId" (Validate.maybe Validate.string))
        (Validate.field "formatUuid" (Validate.maybe Validate.string))


encode : List Permission -> QuestionnaireEditForm -> E.Value
encode permissions form =
    let
        formatUuid =
            Maybe.andThen (always form.formatUuid) form.templateId
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "templateId", E.maybe E.string form.templateId )
        , ( "formatUuid", E.maybe E.string formatUuid )
        , ( "permissions", E.list Permission.encode permissions )
        ]
