module Wizard.Projects.Common.QuestionnaireEditFormPermission exposing
    ( QuestionnaireEditFormPermission
    , encode
    , initFromPermission
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.Permission exposing (Permission)
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Uuid exposing (Uuid)
import Wizard.Projects.Common.QuestionnaireEditFormMember as QuestionnaireEditFormUser exposing (QuestionnaireEditFormMember)
import Wizard.Projects.Common.QuestionnaireEditFormMemberPerms as QuestionnaireEditFormUserPerms exposing (QuestionnaireEditFormMemberPerms)


type alias QuestionnaireEditFormPermission =
    { uuid : Uuid
    , questionnaireUuid : Uuid
    , member : QuestionnaireEditFormMember
    , perms : QuestionnaireEditFormMemberPerms
    }


initFromPermission : Permission -> Field
initFromPermission permission =
    Field.group
        [ ( "uuid", Field.string (Uuid.toString permission.uuid) )
        , ( "questionnaireUuid", Field.string (Uuid.toString permission.questionnaireUuid) )
        , ( "member", QuestionnaireEditFormUser.initFromMember permission.member )
        , ( "perms", QuestionnaireEditFormUserPerms.initFromPerms permission.perms )
        ]


validation : Validation FormError QuestionnaireEditFormPermission
validation =
    V.map4 QuestionnaireEditFormPermission
        (V.field "uuid" V.uuid)
        (V.field "questionnaireUuid" V.uuid)
        (V.field "member" QuestionnaireEditFormUser.validation)
        (V.field "perms" QuestionnaireEditFormUserPerms.validation)


encode : QuestionnaireEditFormPermission -> E.Value
encode permission =
    E.object
        [ ( "uuid", Uuid.encode permission.uuid )
        , ( "questionnaireUuid", Uuid.encode permission.questionnaireUuid )
        , ( "member", QuestionnaireEditFormUser.encode permission.member )
        , ( "perms", QuestionnaireEditFormUserPerms.encode permission.perms )
        ]
