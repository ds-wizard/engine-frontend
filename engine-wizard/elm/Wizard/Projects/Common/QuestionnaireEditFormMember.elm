module Wizard.Projects.Common.QuestionnaireEditFormMember exposing (QuestionnaireEditFormMember, encode, initFromMember, validation)

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.Member exposing (Member)
import Shared.Form.FormError exposing (FormError)
import Uuid


type alias QuestionnaireEditFormMember =
    { uuid : String
    }


initFromMember : Member -> Field
initFromMember member =
    Field.group [ ( "uuid", Field.string (Uuid.toString member.uuid) ) ]


validation : Validation FormError QuestionnaireEditFormMember
validation =
    V.map QuestionnaireEditFormMember
        (V.field "uuid" V.string)


encode : QuestionnaireEditFormMember -> E.Value
encode user =
    E.object
        [ ( "uuid", E.string user.uuid )
        , ( "type", E.string "UserMember" )
        ]
