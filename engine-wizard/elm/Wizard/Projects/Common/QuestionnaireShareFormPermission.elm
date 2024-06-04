module Wizard.Projects.Common.QuestionnaireShareFormPermission exposing
    ( QuestionnaireShareFormPermission
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
import Wizard.Projects.Common.QuestionnaireShareFormMemberPermType as QuestionnaireEditFormUserPerms exposing (QuestionnaireShareFormMemberPermType)
import Wizard.Projects.Common.QuestionnaireShareFormMemberType as QuestionnaireShareFormMemberType exposing (QuestionnaireShareFormMemberType)


type alias QuestionnaireShareFormPermission =
    { memberUuid : Uuid
    , memberType : QuestionnaireShareFormMemberType
    , perms : QuestionnaireShareFormMemberPermType
    }


initFromPermission : Permission -> Field
initFromPermission permission =
    Field.group
        [ ( "memberUuid", Field.string (Uuid.toString (Member.getUuid permission.member)) )
        , ( "memberType", Field.string (QuestionnaireShareFormMemberType.toString (Member.toQuestionnaireEditFormMemberType permission.member)) )
        , ( "perms", QuestionnaireEditFormUserPerms.initFromPerms permission.perms )
        ]


validation : Validation FormError QuestionnaireShareFormPermission
validation =
    V.map3 QuestionnaireShareFormPermission
        (V.field "memberUuid" V.uuid)
        (V.field "memberType" QuestionnaireShareFormMemberType.validation)
        (V.field "perms" QuestionnaireEditFormUserPerms.validation)


encode : QuestionnaireShareFormPermission -> E.Value
encode permission =
    E.object
        [ ( "memberUuid", Uuid.encode permission.memberUuid )
        , ( "memberType", QuestionnaireShareFormMemberType.encode permission.memberType )
        , ( "perms", QuestionnaireEditFormUserPerms.encode permission.perms )
        ]
