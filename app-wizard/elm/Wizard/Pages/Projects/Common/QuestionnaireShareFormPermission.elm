module Wizard.Pages.Projects.Common.QuestionnaireShareFormPermission exposing
    ( QuestionnaireShareFormPermission
    , encode
    , initFromPermission
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Form.Validate as V
import Uuid exposing (Uuid)
import Wizard.Api.Models.Member as Member
import Wizard.Api.Models.Permission exposing (Permission)
import Wizard.Pages.Projects.Common.QuestionnaireShareFormMemberPermType as QuestionnaireEditFormUserPerms exposing (QuestionnaireShareFormMemberPermType)
import Wizard.Pages.Projects.Common.QuestionnaireShareFormMemberType as QuestionnaireShareFormMemberType exposing (QuestionnaireShareFormMemberType)


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
