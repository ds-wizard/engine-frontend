module Wizard.Projects.Common.QuestionnaireEditFormPermission exposing
    ( QuestionnaireEditFormPermission
    , encode
    , initFromPermission
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.Member as Member
import Shared.Data.Permission exposing (Permission)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Uuid exposing (Uuid)
import Wizard.Projects.Common.QuestionnaireEditFormMemberType as QuestionnaireEditFormMemberType exposing (QuestionnaireEditFormMemberType)
import Wizard.Projects.Common.QuestionnaireEditFormQuestionnairePermType as QuestionnaireEditFormUserPerms exposing (QuestionnaireEditFormMemberPerms)


type alias QuestionnaireEditFormPermission =
    { memberUuid : Uuid
    , memberType : QuestionnaireEditFormMemberType
    , perms : QuestionnaireEditFormMemberPerms
    }


initFromPermission : Permission -> Field
initFromPermission permission =
    Field.group
        [ ( "memberUuid", Field.string (Uuid.toString (Member.getUuid permission.member)) )
        , ( "memberType", Field.string (QuestionnaireEditFormMemberType.toString (Member.toQuestionnaireEditFormMemberType permission.member)) )
        , ( "perms", QuestionnaireEditFormUserPerms.initFromPerms permission.perms )
        ]


validation : Validation FormError QuestionnaireEditFormPermission
validation =
    V.map3 QuestionnaireEditFormPermission
        (V.field "memberUuid" V.uuid)
        (V.field "memberType" QuestionnaireEditFormMemberType.validation)
        (V.field "perms" QuestionnaireEditFormUserPerms.validation)


encode : QuestionnaireEditFormPermission -> E.Value
encode permission =
    E.object
        [ ( "memberUuid", Uuid.encode permission.memberUuid )
        , ( "memberType", QuestionnaireEditFormMemberType.encode permission.memberType )
        , ( "perms", QuestionnaireEditFormUserPerms.encode permission.perms )
        ]
