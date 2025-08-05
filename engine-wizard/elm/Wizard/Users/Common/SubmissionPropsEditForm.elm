module Wizard.Users.Common.SubmissionPropsEditForm exposing
    ( SubmissionPropsEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V
import Wizard.Api.Models.SubmissionProps exposing (SubmissionProps)


type alias SubmissionPropsEditForm =
    { submissionProps : List SubmissionProps }


initEmpty : Form FormError SubmissionPropsEditForm
initEmpty =
    Form.initial [] validation


init : List SubmissionProps -> Form FormError SubmissionPropsEditForm
init submissionProps =
    let
        submissionPropsGroups =
            List.map (Field.group << initSubmission) submissionProps

        fields =
            [ ( "submissionProps", Field.list submissionPropsGroups )
            ]
    in
    Form.initial fields validation


initSubmission : SubmissionProps -> List ( String, Field.Field )
initSubmission submission =
    [ ( "id", Field.string submission.id )
    , ( "name", Field.string submission.name )
    , ( "values", Field.dict Field.string submission.values )
    ]


validation : Validation FormError SubmissionPropsEditForm
validation =
    V.succeed SubmissionPropsEditForm
        |> V.andMap (V.field "submissionProps" (V.list validateSubmissionProps))


validateSubmissionProps : Validation FormError SubmissionProps
validateSubmissionProps =
    V.succeed SubmissionProps
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "values" (V.dict V.optionalString))


encode : SubmissionPropsEditForm -> E.Value
encode form =
    let
        encodeSubmission : SubmissionProps -> E.Value
        encodeSubmission submission =
            E.object
                [ ( "id", E.string submission.id )
                , ( "name", E.string submission.name )
                , ( "values", E.dict identity E.string submission.values )
                ]
    in
    E.list encodeSubmission form.submissionProps
