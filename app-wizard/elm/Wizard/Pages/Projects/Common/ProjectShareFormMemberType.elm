module Wizard.Pages.Projects.Common.ProjectShareFormMemberType exposing
    ( ProjectShareFormMemberType(..)
    , encode
    , toString
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as V exposing (Validation)
import Json.Encode as E


type ProjectShareFormMemberType
    = UserProjectPermType
    | UserGroupProjectPermType


toString : ProjectShareFormMemberType -> String
toString memberType =
    case memberType of
        UserProjectPermType ->
            "UserProjectPermType"

        UserGroupProjectPermType ->
            "UserGroupProjectPermType"


encode : ProjectShareFormMemberType -> E.Value
encode =
    E.string << toString


validation : Validation FormError ProjectShareFormMemberType
validation =
    V.string
        |> V.andThen
            (\formPerms ->
                case formPerms of
                    "UserProjectPermType" ->
                        V.succeed UserProjectPermType

                    "UserGroupProjectPermType" ->
                        V.succeed UserGroupProjectPermType

                    _ ->
                        V.fail <| Error.value InvalidString
            )
