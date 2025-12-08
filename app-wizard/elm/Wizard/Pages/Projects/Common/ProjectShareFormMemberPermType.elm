module Wizard.Pages.Projects.Common.ProjectShareFormMemberPermType exposing
    ( ProjectShareFormMemberPermType(..)
    , encode
    , formOptions
    , initFromPerms
    , toString
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Json.Encode as E
import Wizard.Api.Models.ProjectPerm as ProjectPerm
import Wizard.Data.AppState exposing (AppState)


type ProjectShareFormMemberPermType
    = Viewer
    | Commenter
    | Editor
    | Owner


initFromPerms : List String -> Field
initFromPerms perms =
    Field.string <|
        if List.member ProjectPerm.admin perms then
            toString Owner

        else if List.member ProjectPerm.edit perms then
            toString Editor

        else if List.member ProjectPerm.comment perms then
            toString Commenter

        else
            toString Viewer


toString : ProjectShareFormMemberPermType -> String
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


validation : Validation FormError ProjectShareFormMemberPermType
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


encode : ProjectShareFormMemberPermType -> E.Value
encode perms =
    E.list E.string <|
        case perms of
            Owner ->
                [ ProjectPerm.view, ProjectPerm.comment, ProjectPerm.edit, ProjectPerm.admin ]

            Editor ->
                [ ProjectPerm.view, ProjectPerm.comment, ProjectPerm.edit ]

            Commenter ->
                [ ProjectPerm.view, ProjectPerm.comment ]

            Viewer ->
                [ ProjectPerm.view ]
