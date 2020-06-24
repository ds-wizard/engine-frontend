module Wizard.Users.Common.UserEditForm exposing
    ( UserEditForm
    , encode
    , init
    , initEmpty
    , validation
    )

import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (..)
import Json.Encode as E exposing (..)
import Json.Encode.Extra as E
import Shared.Data.User exposing (User)
import Shared.Form.Field as Field
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias UserEditForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , role : String
    , active : Bool
    , submissionProps : List SubmissionProps
    }


type alias SubmissionProps =
    { id : String
    , name : String
    , values : Dict String String
    }


initEmpty : Form FormError UserEditForm
initEmpty =
    Form.initial [] validation


init : User -> Form FormError UserEditForm
init user =
    Form.initial (initUser user) validation


initUser : User -> List ( String, Field.Field )
initUser user =
    let
        submissionProps =
            List.map (Field.group << initSubmission) user.submissionProps
    in
    [ ( "email", Field.string user.email )
    , ( "firstName", Field.string user.firstName )
    , ( "lastName", Field.string user.lastName )
    , ( "affiliation", Field.maybeString user.affiliation )
    , ( "role", Field.string user.role )
    , ( "active", Field.bool user.active )
    , ( "submissionProps", Field.list submissionProps )
    ]


initSubmission : SubmissionProps -> List ( String, Field.Field )
initSubmission submission =
    [ ( "id", Field.string submission.id )
    , ( "name", Field.string submission.name )
    , ( "values", Field.dict Field.string submission.values )
    ]


validation : Validation FormError UserEditForm
validation =
    V.succeed UserEditForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "role" V.string)
        |> V.andMap (V.field "active" V.bool)
        |> V.andMap (V.field "submissionProps" (V.list validateSubmissionProps))


validateSubmissionProps : Validation FormError SubmissionProps
validateSubmissionProps =
    V.succeed SubmissionProps
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "values" (V.dict V.string))


encode : String -> UserEditForm -> E.Value
encode uuid form =
    let
        encodeSubmission : SubmissionProps -> E.Value
        encodeSubmission submission =
            E.object
                [ ( "id", E.string submission.id )
                , ( "name", E.string submission.name )
                , ( "values", E.dict identity E.string submission.values )
                ]
    in
    E.object
        [ ( "uuid", E.string uuid )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "role", E.string form.role )
        , ( "active", E.bool form.active )
        , ( "submissionProps", E.list encodeSubmission form.submissionProps )
        ]
