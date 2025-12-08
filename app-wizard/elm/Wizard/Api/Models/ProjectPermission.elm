module Wizard.Api.Models.ProjectPermission exposing
    ( ProjectPermission(..)
    , field
    , formOptions
    , validation
    )

import Form.Error as Error
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import List.Extra as List


type ProjectPermission
    = View
    | Comment
    | Edit


validation : Validation e ProjectPermission
validation =
    V.string
        |> V.andThen
            (\value ->
                case value of
                    "view" ->
                        V.succeed View

                    "comment" ->
                        V.succeed Comment

                    "edit" ->
                        V.succeed Edit

                    _ ->
                        V.fail <| Error.value Error.InvalidString
            )


toString : ProjectPermission -> String
toString projectPermission =
    case projectPermission of
        View ->
            "view"

        Comment ->
            "comment"

        Edit ->
            "edit"


field : ProjectPermission -> Field
field =
    toString >> Field.string


formOptions : { a | locale : Gettext.Locale } -> Maybe String -> List ( String, String )
formOptions appState mbFilterPerm =
    let
        drop ( perm, _ ) =
            case mbFilterPerm of
                Just filterPerm ->
                    perm /= filterPerm

                Nothing ->
                    False
    in
    List.dropWhile drop
        [ ( "view", gettext "view" appState.locale )
        , ( "comment", gettext "comment" appState.locale )
        , ( "edit", gettext "edit" appState.locale )
        ]
