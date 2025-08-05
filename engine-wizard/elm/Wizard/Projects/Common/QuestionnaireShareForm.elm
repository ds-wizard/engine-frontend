module Wizard.Projects.Common.QuestionnaireShareForm exposing
    ( QuestionnaireShareForm
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
import Shared.Form.FormError exposing (FormError)
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Questionnaire.QuestionnaireSharing as QuestionnaireSharing
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility
import Wizard.Api.Models.QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Api.Models.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Wizard.Projects.Common.QuestionnaireShareFormPermission as QuestionnaireShareFormPermission exposing (QuestionnaireShareFormPermission)


type alias QuestionnaireShareForm =
    { visibilityEnabled : Bool
    , visibilityPermission : QuestionnairePermission
    , sharingEnabled : Bool
    , sharingPermission : QuestionnairePermission
    , permissions : List QuestionnaireShareFormPermission
    }


initEmpty : Form FormError QuestionnaireShareForm
initEmpty =
    Form.initial [] validation


init : QuestionnaireCommon -> Form FormError QuestionnaireShareForm
init questionnaire =
    Form.initial (questionnaireToFormInitials questionnaire) validation


getMemberUuids : Form FormError QuestionnaireShareForm -> List String
getMemberUuids form =
    let
        indexes =
            Form.getListIndexes "permissions" form

        toMemberUuid index =
            Maybe.withDefault "" (Form.getFieldAsString ("permissions." ++ String.fromInt index ++ ".memberUuid") form).value
    in
    List.map toMemberUuid indexes


questionnaireToFormInitials : QuestionnaireCommon -> List ( String, Field.Field )
questionnaireToFormInitials questionnaire =
    let
        ( visibilityEnabled, visibilityPermission ) =
            QuestionnaireVisibility.toFormValues questionnaire.visibility

        ( sharingEnabled, sharingPermission ) =
            QuestionnaireSharing.toFormValues questionnaire.sharing

        permissionFields =
            List.map QuestionnaireShareFormPermission.initFromPermission <|
                List.sortWith (\p1 p2 -> Member.compare p1.member p2.member) questionnaire.permissions
    in
    [ ( "visibilityEnabled", Field.bool visibilityEnabled )
    , ( "visibilityPermission", QuestionnairePermission.field visibilityPermission )
    , ( "sharingEnabled", Field.bool sharingEnabled )
    , ( "sharingPermission", QuestionnairePermission.field sharingPermission )
    , ( "permissions", Field.list permissionFields )
    ]


validation : Validation FormError QuestionnaireShareForm
validation =
    V.succeed QuestionnaireShareForm
        |> V.andMap (V.field "visibilityEnabled" V.bool)
        |> V.andMap (V.field "visibilityPermission" QuestionnairePermission.validation)
        |> V.andMap (V.field "sharingEnabled" V.bool)
        |> V.andMap (V.field "sharingPermission" QuestionnairePermission.validation)
        |> V.andMap (V.field "permissions" (V.list QuestionnaireShareFormPermission.validation))


encode : QuestionnaireShareForm -> E.Value
encode form =
    E.object
        [ ( "visibility", QuestionnaireVisibility.encode (QuestionnaireVisibility.fromFormValues form.visibilityEnabled form.visibilityPermission form.sharingEnabled form.sharingPermission) )
        , ( "sharing", QuestionnaireSharing.encode (QuestionnaireSharing.fromFormValues form.sharingEnabled form.sharingPermission) )
        , ( "permissions", E.list QuestionnaireShareFormPermission.encode form.permissions )
        ]
