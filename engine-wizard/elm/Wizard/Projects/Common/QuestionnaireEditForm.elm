module Wizard.Projects.Common.QuestionnaireEditForm exposing
    ( QuestionnaireEditForm
    , encode
    , getMemberUuids
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
import Shared.Data.Member as Member
import Shared.Data.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Projects.Common.QuestionnaireEditFormPermission as QuestionnaireEditFormPermission exposing (QuestionnaireEditFormPermission)


type alias QuestionnaireEditForm =
    { name : String
    , description : Maybe String
    , projectTags : List String
    , isTemplate : Bool
    , visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    , documentTemplateId : Maybe String
    , formatUuid : Maybe String
    , permissions : List QuestionnaireEditFormPermission
    }


initEmpty : AppState -> Form FormError QuestionnaireEditForm
initEmpty appState =
    Form.initial [] (validation appState)


init : AppState -> QuestionnaireDetail -> Form FormError QuestionnaireEditForm
init appState questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) (validation appState)


getMemberUuids : Form FormError QuestionnaireEditForm -> List String
getMemberUuids form =
    let
        indexes =
            Form.getListIndexes "permissions" form

        toMemberUuid index =
            Maybe.withDefault "" (Form.getFieldAsString ("permissions." ++ String.fromInt index ++ ".memberUuid") form).value
    in
    List.map toMemberUuid indexes


questionnaireToFormInitials : QuestionnaireDetail -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    let
        ( visibilityEnabled, visibilityPermission ) =
            QuestionnaireVisibility.toFormValues questionnaire.visibility

        ( sharingEnabled, sharingPermission ) =
            QuestionnaireSharing.toFormValues questionnaire.sharing

        permissionFields =
            List.map QuestionnaireEditFormPermission.initFromPermission <|
                List.sortWith (\p1 p2 -> Member.compare p1.member p2.member) questionnaire.permissions
    in
    [ ( "name", Field.string questionnaire.name )
    , ( "description", Field.string (Maybe.withDefault "" questionnaire.description) )
    , ( "projectTags", Field.list (List.map Field.string questionnaire.projectTags ++ [ Field.string "" ]) )
    , ( "isTemplate", Field.bool questionnaire.isTemplate )
    , ( "visibilityEnabled", Field.bool visibilityEnabled )
    , ( "visibilityPermission", QuestionnairePermission.field visibilityPermission )
    , ( "sharingEnabled", Field.bool sharingEnabled )
    , ( "sharingPermission", QuestionnairePermission.field sharingPermission )
    , ( "documentTemplateId", Field.string (Maybe.withDefault "" questionnaire.documentTemplateId) )
    , ( "formatUuid", Field.string (Maybe.unwrap "" Uuid.toString questionnaire.formatUuid) )
    , ( "permissions", Field.list permissionFields )
    ]


validation : AppState -> Validation FormError QuestionnaireEditForm
validation appState =
    V.succeed QuestionnaireEditForm
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" (V.maybe V.string))
        |> V.andMap (V.field "projectTags" (V.list (V.oneOf [ V.emptyString, V.projectTag appState ])))
        |> V.andMap (V.field "isTemplate" V.bool)
        |> V.andMap (V.field "visibilityEnabled" V.bool)
        |> V.andMap (V.field "visibilityPermission" QuestionnairePermission.validation)
        |> V.andMap (V.field "sharingEnabled" V.bool)
        |> V.andMap (V.field "sharingPermission" QuestionnairePermission.validation)
        |> V.andMap (V.field "documentTemplateId" (V.maybe V.string))
        |> V.andMap (V.field "formatUuid" (V.maybe V.string))
        |> V.andMap (V.field "permissions" (V.list QuestionnaireEditFormPermission.validation))


encode : QuestionnaireEditForm -> E.Value
encode form =
    let
        formatUuid =
            Maybe.andThen (always form.formatUuid) form.documentTemplateId

        projectTags =
            form.projectTags
                |> List.filter (not << String.isEmpty)
                |> List.sortBy String.toUpper
    in
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.maybe E.string form.description )
        , ( "projectTags", E.list E.string projectTags )
        , ( "isTemplate", E.bool form.isTemplate )
        , ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "documentTemplateId", E.maybe E.string form.documentTemplateId )
        , ( "formatUuid", E.maybe E.string formatUuid )
        , ( "permissions", E.list QuestionnaireEditFormPermission.encode form.permissions )
        ]
