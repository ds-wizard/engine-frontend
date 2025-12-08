module Wizard.Api.Models.Project.ProjectVisibility exposing
    ( ProjectVisibility(..)
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


type ProjectVisibility
    = Private
    | VisibleView
    | VisibleComment
    | VisibleEdit


toString : ProjectVisibility -> String
toString projectVisibility =
    case projectVisibility of
        Private ->
            "PrivateProjectVisibility"

        VisibleView ->
            "VisibleViewProjectVisibility"

        VisibleComment ->
            "VisibleCommentProjectVisibility"

        VisibleEdit ->
            "VisibleEditProjectVisibility"


fromString : String -> Maybe ProjectVisibility
fromString str =
    case str of
        "PrivateProjectVisibility" ->
            Just Private

        "VisibleViewProjectVisibility" ->
            Just VisibleView

        "VisibleCommentProjectVisibility" ->
            Just VisibleComment

        "VisibleEditProjectVisibility" ->
            Just VisibleEdit

        _ ->
            Nothing


encode : ProjectVisibility -> E.Value
encode =
    E.string << toString


decoder : Decoder ProjectVisibility
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just visibility ->
                        D.succeed visibility

                    Nothing ->
                        D.fail <| "Unknown project visibility: " ++ str
            )


toFormValues : ProjectVisibility -> ( Bool, ProjectPermission )
toFormValues sharing =
    case sharing of
        Private ->
            ( False, ProjectPermission.View )

        VisibleView ->
            ( True, ProjectPermission.View )

        VisibleComment ->
            ( True, ProjectPermission.Comment )

        VisibleEdit ->
            ( True, ProjectPermission.Edit )


fromFormValues : Bool -> ProjectPermission -> Bool -> ProjectPermission -> ProjectVisibility
fromFormValues enabled perm visibilityEnabled visibilityPerm =
    if enabled then
        if perm == ProjectPermission.Edit || (visibilityEnabled && visibilityPerm == ProjectPermission.Edit) then
            VisibleEdit

        else if perm == ProjectPermission.Comment || (visibilityEnabled && visibilityPerm == ProjectPermission.Comment) then
            VisibleComment

        else
            VisibleView

    else
        Private


field : ProjectVisibility -> Field
field =
    toString >> Field.string


validation : Validation e ProjectVisibility
validation =
    Validate.string
        |> Validate.andThen
            (\valueType ->
                case valueType of
                    "PrivateProjectVisibility" ->
                        Validate.succeed Private

                    "VisibleViewProjectVisibility" ->
                        Validate.succeed VisibleView

                    "VisibleCommentProjectVisibility" ->
                        Validate.succeed VisibleComment

                    "VisibleEditProjectVisibility" ->
                        Validate.succeed VisibleEdit

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


richFormOptions : { a | locale : Gettext.Locale } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString Private
      , gettext "Private" appState.locale
      , gettext "Visible only to the owner and invited users." appState.locale
      )
    , ( toString VisibleView
      , gettext "Visible - View" appState.locale
      , gettext "Other logged-in users can view the project." appState.locale
      )
    , ( toString VisibleComment
      , gettext "Visible - Comment" appState.locale
      , gettext "Other logged-in users can view and comment the project." appState.locale
      )
    , ( toString VisibleEdit
      , gettext "Visible - Edit" appState.locale
      , gettext "Other logged-in users can edit the project." appState.locale
      )
    ]
