module Wizard.Projects.Common.QuestionnaireShareFormMemberType exposing
    ( QuestionnaireShareFormMemberType(..)
    , encode
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)


type QuestionnaireShareFormMemberType
    = UserQuestionnairePermType
    | UserGroupQuestionnairePermType


toString : QuestionnaireShareFormMemberType -> String
toString memberType =
    case memberType of
        UserQuestionnairePermType ->
            "UserQuestionnairePermType"

        UserGroupQuestionnairePermType ->
            "UserGroupQuestionnairePermType"


encode : QuestionnaireShareFormMemberType -> E.Value
encode =
    E.string << toString


validation : Validation FormError QuestionnaireShareFormMemberType
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
