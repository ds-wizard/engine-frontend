module Wizard.Projects.Common.QuestionnaireEditFormMemberType exposing
    ( QuestionnaireEditFormMemberType(..)
    , encode
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)


type QuestionnaireEditFormMemberType
    = UserQuestionnairePermType
    | UserGroupQuestionnairePermType


toString : QuestionnaireEditFormMemberType -> String
toString memberType =
    case memberType of
        UserQuestionnairePermType ->
            "UserQuestionnairePermType"

        UserGroupQuestionnairePermType ->
            "UserGroupQuestionnairePermType"


encode : QuestionnaireEditFormMemberType -> E.Value
encode =
    E.string << toString


validation : Validation FormError QuestionnaireEditFormMemberType
validation =
    V.string
        |> V.andThen
            (\formPerms ->
                case formPerms of
                    "UserQuestionnairePermType" ->
                        V.succeed UserQuestionnairePermType

                    "UserGroupQuestionnairePermType" ->
                        V.succeed UserGroupQuestionnairePermType

                    _ ->
                        V.fail <| Error.value InvalidString
            )
