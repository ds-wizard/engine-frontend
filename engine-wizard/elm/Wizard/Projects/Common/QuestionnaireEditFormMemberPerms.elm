module Wizard.Projects.Common.QuestionnaireEditFormMemberPerms exposing
    ( QuestionnaireEditFormMemberPerms(..)
    , encode
    , formOptions
    , initFromPerms
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Data.QuestionnairePerm as QuestionnairePerm
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (lg)
import Wizard.Common.AppState exposing (AppState)


type QuestionnaireEditFormMemberPerms
    = Viewer
    | Editor
    | Owner


initFromPerms : List String -> Field
initFromPerms perms =
    Field.string <|
        if List.member QuestionnairePerm.admin perms then
            toString Owner

        else if List.member QuestionnairePerm.edit perms then
            toString Editor

        else
            toString Viewer


toString : QuestionnaireEditFormMemberPerms -> String
toString perms =
    case perms of
        Viewer ->
            "Viewer"

        Editor ->
            "Editor"

        Owner ->
            "Owner"


formOptions : AppState -> List ( String, String )
formOptions appState =
    [ ( toString Viewer, lg "project.role.viewer" appState )
    , ( toString Editor, lg "project.role.editor" appState )
    , ( toString Owner, lg "project.role.owner" appState )
    ]


validation : Validation FormError QuestionnaireEditFormMemberPerms
validation =
    V.string
        |> V.andThen
            (\formPerms ->
                case formPerms of
                    "Viewer" ->
                        V.succeed Viewer

                    "Editor" ->
                        V.succeed Editor

                    "Owner" ->
                        V.succeed Owner

                    _ ->
                        V.fail <| Error.value InvalidString
            )


encode : QuestionnaireEditFormMemberPerms -> E.Value
encode perms =
    E.list E.string <|
        case perms of
            Owner ->
                [ QuestionnairePerm.view, QuestionnairePerm.edit, QuestionnairePerm.admin ]

            Editor ->
                [ QuestionnairePerm.view, QuestionnairePerm.edit ]

            Viewer ->
                [ QuestionnairePerm.view ]
