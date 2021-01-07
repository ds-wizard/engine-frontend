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
import Wizard.Projects.Common.QuestionnaireEditFormMember as QuestionnaireEditFormUser exposing (QuestionnaireEditFormMember)
import Wizard.Projects.Common.QuestionnaireEditFormMemberPerms as QuestionnaireEditFormUserPerms exposing (QuestionnaireEditFormMemberPerms)


type alias QuestionnaireEditFormPermission =
    { member : QuestionnaireEditFormMember
    , perms : QuestionnaireEditFormMemberPerms
    }


initFromPermission : Permission -> Field
initFromPermission permission =
    Field.group
        [ ( "member", QuestionnaireEditFormUser.initFromMember permission.member )
        , ( "perms", QuestionnaireEditFormUserPerms.initFromPerms permission.perms )
        ]


validation : Validation FormError QuestionnaireEditFormPermission
validation =
    V.map2 QuestionnaireEditFormPermission
        (V.field "member" QuestionnaireEditFormUser.validation)
        (V.field "perms" QuestionnaireEditFormUserPerms.validation)


encode : QuestionnaireEditFormPermission -> E.Value
encode permission =
    E.object
        [ ( "member", QuestionnaireEditFormUser.encode permission.member )
        , ( "perms", QuestionnaireEditFormUserPerms.encode permission.perms )
        ]
