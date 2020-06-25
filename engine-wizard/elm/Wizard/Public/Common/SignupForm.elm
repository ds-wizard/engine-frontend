module Wizard.Public.Common.SignupForm exposing
    ( SignupForm
    , encode
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Validate as V exposing (..)
import Json.Encode as E exposing (..)
import Json.Encode.Extra as E
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias SignupForm =
    { email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , password : String
    , passwordConfirmation : String
    , accept : Bool
    }


initEmpty : Form FormError SignupForm
initEmpty =
    Form.initial [] validation


validation : Validation FormError SignupForm
validation =
    V.succeed SignupForm
        |> V.andMap (V.field "email" V.email)
        |> V.andMap (V.field "firstName" V.string)
        |> V.andMap (V.field "lastName" V.string)
        |> V.andMap (V.field "affiliation" V.maybeString)
        |> V.andMap (V.field "password" V.string)
        |> V.andMap (V.field "password" V.string |> V.confirmation "passwordConfirmation")
        |> V.andMap (V.field "accept" validateAcceptField)


validateAcceptField : Field -> Result (Error customError) Bool
validateAcceptField v =
    if Field.asBool v |> Maybe.withDefault False then
        Ok True

    else
        Err (Error.value Empty)


encode : SignupForm -> E.Value
encode form =
    E.object
        [ ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "affiliation", E.maybe E.string form.affiliation )
        , ( "password", E.string form.password )
        , ( "role", E.null )
        ]
