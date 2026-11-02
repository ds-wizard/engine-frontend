module Common.Data.Forms.ProjectShareFormPermission exposing
    ( ProjectShareFormPermission
    , encode
    , initFromPermission
    , validation
    )

import Common.Api.Models.Member as Member
import Common.Api.Models.Permission exposing (Permission)
import Common.Data.Forms.ProjectShareFormMemberPermType as ProjectShareFormMemberPermType exposing (ProjectShareFormMemberPermType)
import Common.Data.Forms.ProjectShareFormMemberType as ProjectShareFormMemberType exposing (ProjectShareFormMemberType)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Uuid exposing (Uuid)


type alias ProjectShareFormPermission =
    { memberUuid : Uuid
    , memberType : ProjectShareFormMemberType
    , perms : ProjectShareFormMemberPermType
    }


initFromPermission : Permission -> Field
initFromPermission permission =
    Field.group
        [ ( "memberUuid", Field.string (Uuid.toString (Member.getUuid permission.member)) )
        , ( "memberType", Field.string (ProjectShareFormMemberType.toString (Member.toQuestionnaireEditFormMemberType permission.member)) )
        , ( "perms", ProjectShareFormMemberPermType.initFromPerms permission.perms )
        ]


validation : Validation FormError ProjectShareFormPermission
validation =
    V.map3 ProjectShareFormPermission
        (V.field "memberUuid" V.uuid)
        (V.field "memberType" ProjectShareFormMemberType.validation)
        (V.field "perms" ProjectShareFormMemberPermType.validation)


encode : ProjectShareFormPermission -> E.Value
encode permission =
    E.object
        [ ( "memberUuid", Uuid.encode permission.memberUuid )
        , ( "memberType", ProjectShareFormMemberType.encode permission.memberType )
        , ( "perms", ProjectShareFormMemberPermType.encode permission.perms )
        ]
