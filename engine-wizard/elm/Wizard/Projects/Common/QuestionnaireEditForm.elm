module Wizard.Projects.Common.QuestionnaireEditForm exposing
    ( QuestionnaireEditForm
    , encode
    , getUserUuids
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Shared.Data.User as User
import Shared.Form.FormError exposing (FormError)
import Uuid
import Wizard.Projects.Common.QuestionnaireEditFormPermission as QuestionnaireEditFormPermission exposing (QuestionnaireEditFormPermission)


type alias QuestionnaireEditForm =
    { name : String
    , description : Maybe String
    , isTemplate : Bool
    , visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    , templateId : Maybe String
    , formatUuid : Maybe String
    , permissions : List QuestionnaireEditFormPermission
    }


initEmpty : Form FormError QuestionnaireEditForm
initEmpty =
    Form.initial [] validation


init : QuestionnaireDetail -> Form FormError QuestionnaireEditForm
init questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) validation


getUserUuids : Form FormError QuestionnaireEditForm -> List String
getUserUuids form =
    let
        indexes =
            Form.getListIndexes "permissions" form

        toUserUuid index =
            Maybe.withDefault "" (Form.getFieldAsString ("permissions." ++ String.fromInt index ++ ".member.uuid") form).value
    in
    List.map toUserUuid indexes


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    let
        ( visibilityEnabled, visibilityPermission ) =
            QuestionnaireVisibility.toFormValues questionnaire.visibility

        ( sharingEnabled, sharingPermission ) =
            QuestionnaireSharing.toFormValues questionnaire.sharing

        permissionFields =
            List.map QuestionnaireEditFormPermission.initFromPermission <|
                List.sortWith (\p1 p2 -> User.compare p1.member p2.member) questionnaire.permissions
    in
    [ ( "name", Field.string questionnaire.name )
    , ( "description", Field.string (Maybe.withDefault "" questionnaire.description) )
    , ( "isTemplate", Field.bool questionnaire.isTemplate )
    , ( "visibilityEnabled", Field.bool visibilityEnabled )
    , ( "visibilityPermission", QuestionnairePermission.field visibilityPermission )
    , ( "sharingEnabled", Field.bool sharingEnabled )
    , ( "sharingPermission", QuestionnairePermission.field sharingPermission )
    , ( "templateId", Field.string (Maybe.withDefault "" questionnaire.templateId) )
    , ( "formatUuid", Field.string (Maybe.unwrap "" Uuid.toString questionnaire.formatUuid) )
    , ( "permissions", Field.list permissionFields )
    ]


validation : Validation FormError QuestionnaireEditForm
validation =
    V.succeed QuestionnaireEditForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" (V.maybe V.string))
        |> V.andMap (V.field "isTemplate" V.bool)
        |> V.andMap (V.field "visibilityEnabled" V.bool)
        |> V.andMap (V.field "visibilityPermission" QuestionnairePermission.validation)
        |> V.andMap (V.field "sharingEnabled" V.bool)
        |> V.andMap (V.field "sharingPermission" QuestionnairePermission.validation)
        |> V.andMap (V.field "templateId" (V.maybe V.string))
        |> V.andMap (V.field "formatUuid" (V.maybe V.string))
        |> V.andMap (V.field "permissions" (V.list QuestionnaireEditFormPermission.validation))


encode : QuestionnaireEditForm -> E.Value
encode form =
    let
        formatUuid =
            Maybe.andThen (always form.formatUuid) form.templateId
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.maybe E.string form.description )
        , ( "isTemplate", E.bool form.isTemplate )
        , ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "templateId", E.maybe E.string form.templateId )
        , ( "formatUuid", E.maybe E.string formatUuid )
        , ( "permissions", E.list QuestionnaireEditFormPermission.encode form.permissions )
        ]
