module Shared.Data.QuestionnairePermission exposing
    ( QuestionnairePermission(..)
    , field
    , formOptions
    , validation
    )

import Form.Error as Error
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Shared.Locale exposing (lg)
import Shared.Provisioning exposing (Provisioning)


type QuestionnairePermission
    = View
    | Comment
    | Edit


validation : Validation e QuestionnairePermission
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


toString : QuestionnairePermission -> String
toString questionnaireEditability =
    case questionnaireEditability of
        View ->
            "view"

        Comment ->
            "comment"

        Edit ->
            "edit"


field : QuestionnairePermission -> Field
field =
    toString >> Field.string


formOptions : { a | provisioning : Provisioning } -> List ( String, String )
formOptions appState =
    [ ( "view", lg "questionnairePermission.view" appState )
    , ( "comment", lg "questionnairePermission.comment" appState )
    , ( "edit", lg "questionnairePermission.edit" appState )
    ]
