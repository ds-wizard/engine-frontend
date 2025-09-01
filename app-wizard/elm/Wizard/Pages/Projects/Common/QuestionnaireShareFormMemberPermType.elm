module Wizard.Pages.Projects.Common.QuestionnaireShareFormMemberPermType exposing
    ( QuestionnaireShareFormMemberPermType(..)
    , encode
    , formOptions
    , initFromPerms
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Json.Encode as E
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.QuestionnairePerm as QuestionnairePerm
import Wizard.Data.AppState exposing (AppState)


type QuestionnaireShareFormMemberPermType
    = Viewer
    | Commenter
    | Editor
    | Owner


initFromPerms : List String -> Field
initFromPerms perms =
    Field.string <|
        if List.member QuestionnairePerm.admin perms then
            toString Owner

        else if List.member QuestionnairePerm.edit perms then
            toString Editor

        else if List.member QuestionnairePerm.comment perms then
            toString Commenter

        else
            toString Viewer


toString : QuestionnaireShareFormMemberPermType -> String
toString perms =
    case perms of
        Viewer ->
            "Viewer"

        Commenter ->
            "Commenter"

        Editor ->
            "Editor"

        Owner ->
            "Owner"


formOptions : AppState -> List ( String, String )
formOptions appState =
    [ ( toString Viewer, gettext "Viewer" appState.locale )
    , ( toString Commenter, gettext "Commenter" appState.locale )
    , ( toString Editor, gettext "Editor" appState.locale )
    , ( toString Owner, gettext "Owner" appState.locale )
    ]


validation : Validation FormError QuestionnaireShareFormMemberPermType
validation =
    V.string
        |> V.andThen
            (\formPerms ->
                case formPerms of
                    "Viewer" ->
                        V.succeed Viewer

                    "Commenter" ->
                        V.succeed Commenter

                    "Editor" ->
                        V.succeed Editor

                    "Owner" ->
                        V.succeed Owner

                    _ ->
                        V.fail <| Error.value InvalidString
            )


encode : QuestionnaireShareFormMemberPermType -> E.Value
encode perms =
    E.list E.string <|
        case perms of
            Owner ->
                [ QuestionnairePerm.view, QuestionnairePerm.comment, QuestionnairePerm.edit, QuestionnairePerm.admin ]

            Editor ->
                [ QuestionnairePerm.view, QuestionnairePerm.comment, QuestionnairePerm.edit ]

            Commenter ->
                [ QuestionnairePerm.view, QuestionnairePerm.comment ]

            Viewer ->
                [ QuestionnairePerm.view ]
