module Wizard.Api.Models.Project.ProjectSharing exposing
    ( ProjectSharing(..)
    , decoder
    , encode
    , field
    , fromFormValues
    , richFormOptions
    , toFormValues
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (Validation)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.ProjectPermission as ProjectPermission exposing (ProjectPermission)


type ProjectSharing
    = Restricted
    | AnyoneWithLinkView
    | AnyoneWithLinkComment
    | AnyoneWithLinkEdit


toString : ProjectSharing -> String
toString projectSharing =
    case projectSharing of
        Restricted ->
            "RestrictedProjectSharing"

        AnyoneWithLinkView ->
            "AnyoneWithLinkViewProjectSharing"

        AnyoneWithLinkComment ->
            "AnyoneWithLinkCommentProjectSharing"

        AnyoneWithLinkEdit ->
            "AnyoneWithLinkEditProjectSharing"


encode : ProjectSharing -> E.Value
encode =
    E.string << toString


decoder : Decoder ProjectSharing
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "RestrictedProjectSharing" ->
                        D.succeed Restricted

                    "AnyoneWithLinkViewProjectSharing" ->
                        D.succeed AnyoneWithLinkView

                    "AnyoneWithLinkCommentProjectSharing" ->
                        D.succeed AnyoneWithLinkComment

                    "AnyoneWithLinkEditProjectSharing" ->
                        D.succeed AnyoneWithLinkEdit

                    valueType ->
                        D.fail <| "Unknown project sharing: " ++ valueType
            )


toFormValues : ProjectSharing -> ( Bool, ProjectPermission )
toFormValues sharing =
    case sharing of
        Restricted ->
            ( False, ProjectPermission.View )

        AnyoneWithLinkView ->
            ( True, ProjectPermission.View )

        AnyoneWithLinkComment ->
            ( True, ProjectPermission.Comment )

        AnyoneWithLinkEdit ->
            ( True, ProjectPermission.Edit )


fromFormValues : Bool -> ProjectPermission -> ProjectSharing
fromFormValues enabled perm =
    if enabled then
        if perm == ProjectPermission.Edit then
            AnyoneWithLinkEdit

        else if perm == ProjectPermission.Comment then
            AnyoneWithLinkComment

        else
            AnyoneWithLinkView

    else
        Restricted


field : ProjectSharing -> Field
field =
    toString >> Field.string


validation : Validation e ProjectSharing
validation =
    Validate.string
        |> Validate.andThen
            (\valueType ->
                case valueType of
                    "RestrictedProjectSharing" ->
                        Validate.succeed Restricted

                    "AnyoneWithLinkViewProjectSharing" ->
                        Validate.succeed AnyoneWithLinkView

                    "AnyoneWithLinkCommentProjectSharing" ->
                        Validate.succeed AnyoneWithLinkComment

                    "AnyoneWithLinkEditProjectSharing" ->
                        Validate.succeed AnyoneWithLinkEdit

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


richFormOptions : { a | locale : Gettext.Locale } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString Restricted
      , gettext "Restricted" appState.locale
      , gettext "Only logged-in users can access the project depending on the project visibility." appState.locale
      )
    , ( toString AnyoneWithLinkView
      , gettext "View with the link" appState.locale
      , gettext "Anyone on the internet with the link can view." appState.locale
      )
    , ( toString AnyoneWithLinkComment
      , gettext "Comment with the link" appState.locale
      , gettext "Anyone on the internet with the link can view and comment." appState.locale
      )
    , ( toString AnyoneWithLinkEdit
      , gettext "Edit with the link" appState.locale
      , gettext "Anyone on the internet with the link can edit." appState.locale
      )
    ]
